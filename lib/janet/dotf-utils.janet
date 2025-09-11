(defn run<
  ```
  Run a subprocess with command and arguments specified by `args`. Sends to its
  standard input the data in `to-write`.

  TODO: clarify that `to-write` can either be a function or a string/buffer
  TODO: `stdin`, `stdout` and `stderr` parameters - only create pipes for these when needed
  ```
  [args &opt to-write]

  (cond
    (nil? to-write)
    (run< args (fn [_]))

    (or (string? to-write) (buffer? to-write))
    (run< args |(:write $ to-write))

    (function? to-write)
    (let [write-func to-write
          [stdin-r stdin-w] (os/pipe)
          [stdout-r stdout-w] (os/pipe)
          out-buf @""]
      (defer
        (do
          # if we don't close these, the program may block with clogged pipes
          (:close stdin-w)
          (:close stdout-r))

        # write fiber
        (ev/spawn
          (write-func stdin-w))

        # read fiber
        (ev/spawn
          (while (def buf (:read stdout-r 4096))
            (buffer/push-string out-buf buf)))

        (def code
          (os/execute args :p {:in stdin-r :out stdout-w}))

        (if (= code 0)
          out-buf
          nil)))

    (error (string/format "couldn't figure out how to write with: %j"))
    ))

(defn exec-exit
  "Execute the program `args` and exit with its exit code."
  [args]
  (-> (os/execute args :p) (os/exit)))

(defn fzagnostic
  ```
  Invokes fzagnostic with the provided arguments.
  On success, returns a tuple `[n s]` (where `n` is the index of the choice and
  `s` is the text string); on failure/cancellation, returns nil.
  ```
  [choices &named prompt starting-number]

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
            (string/format num-str) (error))))))

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
    (-> stdout-r (:read math/int32-max))))

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
