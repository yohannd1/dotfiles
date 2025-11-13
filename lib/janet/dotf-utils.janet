(defn run<
  "Run a subprocess and communicate with it.
  Starts it with the program name and arguments from `args`, and receives options in `env`.

  Valid options are :in, :out and :err, referring to said descriptors, and they can be one of either:
  - a string/buffer, which is read onto (or written into);
  - a function, which receives the file descriptor;"
  [args &opt env]

  # FIXME: improve this ugly fuck ass spaghetti code
  # TODO: fix the doc. It's not clear.

  (default env {})

  (defn make-reader-fn [kw]
    (def ei (get env kw))
    (cond
      (nil? ei) nil

      (true? ei)
      (fn [pipe]
        (def buf @"")
        (ev/spawn
          (while (def x (:read pipe 4096))
            (buffer/push-string buf x)))
        buf)

      (function? ei)
      (fn [pipe]
        (ev/spawn
          (defer (:close pipe)
            (ei pipe))))

      (error (string/format "bad argument of type %j" (type ei)))))

  (defn make-writer-fn []
    (def ei (get env :in))
    (cond
      (nil? ei) nil

      (or (string? ei) (buffer? ei))
      (fn [pipe]
        (ev/spawn
          (defer (:close pipe)
            (:write pipe ei))))

      (function? ei)
      (fn [pipe]
        (ev/spawn
          (defer (:close pipe)
            (ei pipe))))

      (error (string/format "bad argument: %j" ei))))

  (def in-fn (make-writer-fn))
  (def out-fn (make-reader-fn :out))
  (def err-fn (make-reader-fn :err))

  (def spawn-args @{})
  (when out-fn
    (set (spawn-args :out) :pipe))
  (when err-fn
    (set (spawn-args :err) :pipe))
  (when in-fn
    (set (spawn-args :in) :pipe))

  (def proc
    (os/spawn args :p spawn-args))

  (def ret @{})
  (when out-fn
    (set (ret :out) (out-fn (in proc :out))))
  (when err-fn
    (set (ret :err) (err-fn (in proc :err))))
  (when in-fn
    (in-fn (in proc :in)))

  (defn try-close [x]
    (when x (:close x)))

  (defer
    (do
      (try-close (in proc :out))
      (try-close (in proc :err)))
    (set (ret :code) (:wait proc))
    ret))

(defn run<stdout
  "Runs the specified command and returns either its stdout (if successful) or nil (if not successful)"
  [args]

  (match (run< args {:out true})
    {:code 0 :out x} x
    _ nil))

(defn exec-then-exit
  "Executes the program `args` and exits with its exit code."
  [args]
  (-> (os/execute args :p) (os/exit)))

(defn exec-replace
  "Executes the program `args` through POSIX `exec`, if available.
  If not available, forwards to exec-then-exit."
  [args]
  (compif (dyn 'os/posix-exec)
    (os/posix-exec args :p)
    (exec-then-exit args)))

(defn fzagnostic
  ```
  Invokes fzagnostic with the provided arguments.

  On success, returns a tuple `[n s]` (where `n` is the index of the choice and
  `s` is the text string); on failure/cancellation, returns nil.
  ```
  [choices &named prompt starting-number error-func]

  (default error-func error)
  (default starting-number 0)

  (def entry-fmt
    (let [last-idx (dec (+ starting-number (length choices)))
          idx-string-len (-> last-idx (string) (length))]
      (string "%0" idx-string-len "d || %s\n")))

  (var arglist ["fzagnostic"])
  (unless (nil? prompt)
    (set arglist (tuple ;arglist "-p" prompt)))

  (defn write [f]
    (for i 0 (length choices)
      (def str (string/format entry-fmt (+ i starting-number) (in choices i)))
      (:write f str)))

  (as?->
    (run< arglist {:in write :out true}) .x
    (match .x
      {:code 0 :out data} (string/trim data)
      _ nil)
    (let [[num-str rest] (string/split " " .x 0 2)]
      (if-let [num (scan-number num-str)]
        [(- num starting-number) rest]
        (-> "bad input (%j is not a number)"
            (string/format num-str) (error-func))))))

(defn readline-agnostic
  "Invokes readline-agnostic with the provided arguments.
  Returns the input (as a string) on success, nil on failure/cancellation."
  [&named prompt-msg]

  (def prompt-arg
    (if (nil? prompt-msg) [] ["-p" prompt-msg]))

  (match (run<stdout ["readline-agnostic" ;prompt-arg])
    {:code 0 :out out} out
    _ nil))

(defmacro with-cwd
  "Executes `body` with `cwd` as the current working directory."
  [cwd & body]

  (def v-orig (gensym))
  ~(do
     (def ,v-orig (os/cwd))
     (os/cd ,cwd)
     (defer (os/cd ,v-orig) ,;body)))

(defn die
  "Print a formatted error message and exit with a code."
  [code fmt & args]

  (eprinf "error: ")
  (eprintf fmt ;args)
  (os/exit code))

(defmacro try-brief
  "Shorthand for trying something and quitting if it errors out."
  [& body]

  ~(try
     (do ,;body)
     ([err] (die 1 "%s" err))))

(defn acquire-lock
  "Attempt to acquire a lock. Waits for it to be available, and returns a struct with a :unlock method, which can be used to free the lock."
  [file]

  (assert (string? file))
  (def cmdline ["flock" file "sh" "-c" "echo x; read _"])
  (def proc (os/spawn cmdline :px {:in :pipe :out :pipe}))
  (def {:out p-out} proc)

  (:read p-out 1)
  (:close p-out)

  (defn unlock [self]
    (when (self :locked)
      (set (self :locked) false)
      (let [{:in p-in} (self :proc)]
        (:write p-in "ok\n")
        (:close p-in))))

  @{:proc proc
    :locked true
    :unlock unlock})

(defmacro with-lock
  "Runs `body` in a locked scope of `file`."
  [file & body]

  (with-syms [lock]
    ~(let [,lock (acquire-lock ,file)]
       (defer (:unlock ,lock)
         ,;body))))

(defn make-set
  "Make a set whose entries are `entries`."
  [entries]

  (def ret @{})
  (each x entries
    (set (ret x) true))
  (table/to-struct ret))

(defn getenv-or-die
  "Get the value of an environment variable, or die if it does not exist."
  [name]

  (def val (os/getenv name))
  (if (nil? val)
    (die 1 "env var \"%s\" is not set" name)
    val))

# Adapted from https://github.com/janet-lang/janet/discussions/1211#discussioncomment-6339830
(defn stream-lines
  "Return a coroutine that iterates through lines of a stream.It is the stream equivalent to file/lines."
  [stream &opt chunk-size]

  (default chunk-size 2048)
  (coro
    (var buf @"")
    (var start 0)
    (while (:read stream chunk-size buf)
      (while (def end (string/find "\n" buf start))
        (def result (string/slice buf 0 end))
        (set buf (buffer/slice buf (inc end)))
        (set start 0)
        (yield result))
      (set start (length buf)))
    (when (> start 0)
      (yield (string buf)))))
