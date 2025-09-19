(defn run<
  ```
  Run a subprocess and communicate with it.

  Starts it with the program name and arguments from `args`, sends input data from `to-write`, and returns its output data.

  Note that, when passed, `to-write` can be either a function that receives the writable end of the pipe, or a string/buffer which is fed to the pipe itself.
  ```
  # TODO: `stdin`, `stdout` and `stderr` parameters - only create pipes for these when needed
  [args &opt to-write]

  (cond
    (nil? to-write)
    (run< args (fn [_]))

    (or (string? to-write) (buffer? to-write))
    (run< args |(:write $ to-write))

    (function? to-write)
    (let [write-fn to-write
          proc (os/spawn args :p {:in :pipe :out :pipe})
          {:in p-in :out p-out} proc
          out-buf @""]
      (defer (:close p-out)
        # write fiber
        (ev/spawn
          (defer (:close p-in)
            (write-fn p-in)))

        # read fiber
        (ev/spawn
          (while (def buf (:read p-out 4096))
            (buffer/push-string out-buf buf)))

        (as->
          (:wait proc) .x
          (if (= .x 0) out-buf nil))))

    (error (string/format "couldn't figure out how to write with: %j"))
    ))

(defn exec-exit
  "Execute the program `args` and exit with its exit code. Calls POSIX `exec` if available."
  [args]

  (compif (dyn 'os/posix-exec)
    (os/posix-exec args :p)
    (-> (os/execute args :p) (os/exit))))

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
    (run< arglist write) .x
    (string/trim .x)
    (let [[num-str rest] (string/split " " .x 0 2)]
      (if-let [num (scan-number num-str)]
        [(- num starting-number) rest]
        (-> "bad input (%j is not a number)"
            (string/format num-str) (error-func))))))

(defn readline-agnostic
  ```
  Invokes readline-agnostic with the provided arguments.
  Returns the input (as a string) on success, nil on failure/cancellation.
  ```
  [&named prompt-msg]

  (def prompt-arg
    (if (nil? prompt-msg) [] ["-p" prompt-msg]))

  (def args ["readline-agnostic" ;prompt-arg])
  (def [stdout-r stdout-w] (os/pipe))
  (def code (os/execute args :p {:out stdout-w}))
  (when (= code 0)
    (-> (:read stdout-r math/int32-max) (string/trim))))

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
