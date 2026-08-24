(defmacro- let-rec [name bindings & body]
  (var argnames [])
  (var arginits [])

  (assert (even? (length bindings)))

  (var i 0)
  (while (< i (length bindings))
    (set argnames [;argnames (in bindings i)])
    (set arginits [;arginits (in bindings (inc i))])
    (+= i 2))

  ~(do
     (defn ,name ,argnames
       ,;body)
     (,name ,;arginits)))

(assert
  (= [5 5]
     (let-rec rec [x 0 y 10]
       (if (< x y) (rec (inc x) (dec y)) [x y]))))

(defn parse-args
  ```
  Parse command-line arguments. This parser supports positional arguments, with
  zero or more single-argument names and optionally a last name for zero or
  more arguments.

  It also supports subcommands, but these cannot be nested and are always the
  first positional argument.

  Lastly, it supports options (both long and short), which can receive either
  one argument or none at all.

  Long options follow the "--option" syntax, and short options follow the "-o"
  syntax. In both cases, the option argument must be the next one.
  "--option=value" and "-ovalue" are not supported.

  There's support for the "--" special option, which disables option handling
  for every CLI argument after it. There's also support for the "--help"
  special flag to show auto-generated help text.

  **FUNCTION ARGUMENTS**

  `in-args` are the command-line arguments (the first argument is the program
  name).

  `positional` specifies the structure of the positional arguments, while
  `subcommands` consists of an array of such structures. They are mutually
  exclusive arguments.

  `positional :: nil | {:args (array symbol) :rest (nil | symbol)}`

  `subcommands :: nil | (array {:name symbol :args (array symbol) :rest (nil |
  symbol) :help (nil | string)})`

  `options :: nil | (array {:name symbol :short (nil | symbol) :long (nil |
  symbol) :help (nil | string)})`
  ```
  [&named in-args description positional subcommands options]

  (when (all truthy? [positional subcommands])
    (error "must specify only one of `positional` and `subcommands`"))

  (def progname (->> (in in-args 0) (string/split "/") (last)))

  (defn valid-name? [x]
    (def peg
      (comptime (peg/compile '(* :a (some (+ :w "-")) -1))))
    (and (bytes? x) (truthy? (peg/match peg x))))

  (defn normalize-name [x]
    (assert (valid-name? x) (string/format "not a valid name: %j" x))
    (symbol x))

  (def subcmd-map @{})
  (loop [:when subcommands
         subcmd :in subcommands]
    (def {:name name} subcmd)
    (def name (normalize-name name))
    (set (subcmd-map name) subcmd))

  (default options [])
  (def option-map @{})
  (def option-kw-map @{})
  (each option options
    (def {:name name :short short :long long} option)
    (def name (normalize-name name))
    (when short (set (option-kw-map short) name))
    (when long (set (option-kw-map long) name))
    (set (option-map name) option))

  (defn show-help [&opt msg]
    (when msg
      (eprintf "error: %s" msg))
    (when description
      (eprintf "%s: %s" progname description))
    (defn print-argspec [args rest]
      (each u (map string/ascii-upper args)
        (eprinf " <%s>" u))
      (unless (nil? rest)
        (eprinf " [%s...]" (string/ascii-upper rest))))
    (unless (nil? positional)
      (eprinf "\nUsage: %s" progname)
      (print-argspec (in positional :args) (in positional :rest))
      (eprintf ""))
    (unless (nil? subcommands)
      (eprintf "\nUsage:")
      (each {:name name :args args :rest rest :help help} subcommands
        (eprinf "  %s %s" progname name)
        (print-argspec args rest)
        (eprintf "")
        (unless (nil? help)
          (eprintf "    %s" help))))
    (unless (empty? options)
      (eprintf "\nOptions:")
      (each {:name name :short short :long long :help help} options
        (def cand [;(if short [short] []) ;(if long [long] [])])
        (eprinf "  %s" (string/join cand ", "))
        (unless (nil? help)
          (eprinf ": %s" help))
        (eprintf "")))
    (os/exit 2))

  (defn is-short-option [x]
    (def p (comptime (peg/compile '(* "-" :a))))
    (truthy? (peg/match p x)))

  (defn is-long-option [x]
    (def p (comptime (peg/compile '(* "--" :a (any (+ :w "-"))))))
    (truthy? (peg/match p x)))

  (assert (is-short-option "-h"))
  (assert (is-long-option "--help"))

  (defn is-option [x]
    (or (is-short-option x) (is-long-option x)))

  (var in-i 1)
  (var skip-opt false)
  (var min-args nil)
  (var max-args nil)
  (var cur-subcmd nil)
  (var subcmd-name nil)

  (defn set-subcmd [subcmd name]
    (def {:args args :rest rest} subcmd)

    (set min-args (length args))
    (set max-args (if (nil? rest) min-args math/inf))
    (set cur-subcmd subcmd)
    (set subcmd-name name))

  (cond
    (not (nil? positional))
    (set-subcmd positional nil)

    (not (nil? subcommands))
    nil # do nothing; subcommand will be picked later

    # make a dummy subcommand with no arguments
    (set-subcmd {:args []}))

  (defn no-more-args? []
    (>= in-i (length in-args)))

  (defn get-option-arg []
    (when skip-opt
      (break nil))

    (def a0 (in in-args in-i))
    (unless (is-option a0)
      (break nil))

    # TODO: make this configurable?
    (when (= a0 "--help")
      (show-help))

    (def name (in option-kw-map a0))

    (def info (assert (in option-map name)))

    (++ in-i)
    (unless (in info :has-arg)
      (pp ~(opt ,name))
      (break [name]))

    (when (>= in-i (length in-args))
      (show-help (string "option " a0 " expected an argument")))

    (++ in-i)
    (def a1 (in in-args in-i))
    (pp ~(opt ,name ,a1))
    [name a1])

  (defn get-pos-arg []
    (def a0 (in in-args in-i))
    (when (and (not skip-opt) (is-option a0))
      (break nil))
    (++ in-i)
    (comment (pp ~(pos-arg ,a0)))
    a0)

  (defn get-skip-arg []
    (when skip-opt
      (break false))
    (def a0 (in in-args in-i))
    (unless (= a0 "--")
      (break false))
    (++ in-i)
    (comment (pp ~(skip-arg)))
    true)

  (var n-provided 0)
  (def provided @{})
  (while (not (no-more-args?))
    (cond
      (get-skip-arg)
      (set skip-opt true)

      (def pa-raw (get-pos-arg))
      (if (nil? cur-subcmd)
        (let [pa-sym (symbol pa-raw)]
          (if (def sc (in subcmd-map pa-sym))
            (set-subcmd sc pa-sym)
            (show-help (string "invalid subcommand: " pa-sym))))
        (let [{:args sc-args} cur-subcmd]
          (cond
            (= n-provided max-args)
            (show-help "too many arguments")

            # named args
            (< n-provided (length sc-args))
            (set (provided (in sc-args n-provided)) pa-raw)

            # "rest" args
            (let [{:rest sc-rest} cur-subcmd]
              (unless (in provided sc-rest)
                (set (provided sc-rest) @[]))
              (array/push (in provided sc-rest) pa-raw)))
          (++ n-provided)))

      (def oa (get-option-arg))
      (comment (pp ~(opt ,oa)))))

  (when (< n-provided min-args)
    (show-help "not enough args"))

  # TODO: this shouldn't always error out, should it?
  (when (nil? cur-subcmd)
    (show-help "no subcommand provided"))

  {:args provided
   :subcmd subcmd-name})
