(defn fzagnostic
  ```
  Invokes fzagnostic with the provided arguments.

  On success, returns a tuple `[n s]` where n is the index of the choice and s is the text string.

  On failure/cancellation, returns nil.
  ```
  [choices &named prompt starting-number]

  (def [stdin-r stdin-w] (os/pipe))
  (def [stdout-r stdout-w] (os/pipe))

  (def starting-number (or starting-number 0))
  (def fmt (let [max-n-len (-> starting-number (+ (length choices)) (- 1) (string) (length))]
             (string "%0" max-n-len "d || %s\n")))

  # write choices to stdin (separate fiber)
  (ev/spawn
    (for i 0 (length choices)
      (def str (string/format fmt (+ i starting-number) (in choices i)))
      (:write stdin-w str))
    (:close stdin-w))

  (def arglist @["fzagnostic"])
  (unless (nil? prompt)
    (array/push arglist "-p")
    (array/push arglist prompt))

  (def code (os/execute arglist :p {:in stdin-r :out stdout-w}))
  (when (= code 0)
    (def choice-raw (-> stdout-r (:read math/int32-max) (string/trim)))
    (def [num-str rest] (string/split " " choice-raw 0 2))
    (if-let [num (scan-number num-str)]
      [(- num starting-number) rest]
      (-> "bad input (%j is not a number)" (string/format num-str) (error)))
    )
  )

(defmacro with-cwd
  "Executes `body` with `cwd` as the current working directory."
  [cwd & body]

  (def v-orig (gensym))
  ~(do
     (def ,v-orig (os/cwd))
     (os/cd ,cwd)
     (defer (os/cd ,v-orig) ,;body)
     ))

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
    ([err] (die 1 "%s" err)))
  )
