(use dotf-utils)
(use dotf-path)

(defn acr-make-parser [fd]
  # Valid states:
  #   'start : nothing parsed yet
  #   'did-header : parsed the header
  #   'done : parsed header & body
  (var state 'start)

  (def lines-iter (file/lines fd))
  (var last-lnum 0)
  (defn next-line []
    (def k (next lines-iter))
    (unless (nil? k)
      (++ last-lnum)
      (in lines-iter k)))

  # declaring it early because parse-header might add to it
  (def body @"")
  (var body-start-lnum nil)

  (defn parse-header [_]
    (when (not= state 'start)
      (error "header already parsed/skipped"))

    (def entries @{})

    (defn is-tag-line [x]
      (string/has-prefix? "%:" x))

    (letrec rec []
      (def line (next-line))

      (if (and line (is-tag-line line))
        (let [[start ev] (string/split " " line 0 2)
              ek (-> start (string/slice 2) keyword)]
          (set (entries ek) (string/trim ev))
          (rec))
        (do
          (when line
            (buffer/push-string body line))
          (set body-start-lnum last-lnum))))
    (set state 'did-header)

    entries)

  (defn parse-body [_]
    (when (not= state 'did-header)
      (error "invalid state for parsing body"))

    # push everything in the body into a buffer
    (while (def line (next-line))
      (buffer/push-string body line)
      (each line lines-iter
        (buffer/push-string body line)))
    (set state 'done)

    body)

  (defn get-body-start-lnum [_]
    (when (= state 'start)
      (error "info not available yet"))
    body-start-lnum)

  {:parse-header parse-header
   :parse-body parse-body
   :get-body-start-lnum get-body-start-lnum})

(defn acr-parse-file [fd &named only-header]
  (def ret @{})
  (def p (acr-make-parser fd))

  (set (ret :header) (:parse-header p))
  (set (ret :body-start-lnum) (:get-body-start-lnum p))
  (unless only-header
    (set (ret :body) (:parse-body p)))
  ret)

(defn acr-read-header [fd]
  (in (acr-parse-file fd :only-header true) :header))

(defn acr-read-contents [fd]
  (in (acr-parse-file fd) :body))

(defn acr-slurp-header [path]
  (with [fd (file/open path :rn)]
    (acr-read-header fd)))

(defn acr-slurp-contents [path]
  (with [fd (file/open path :rn)]
    (acr-read-contents fd)))

(defn acr-slurp-parse [path & args]
  (with [fd (file/open path :rn)]
    (acr-parse-file fd ;args)))

(defn acr-get-wiki-note-pairs [wiki-dir]
  (seq [name :in (os/dir wiki-dir)
        :let [suffix ".acr"]
        :when (string/has-suffix? suffix name)
        :let [id (string/slice name 0 (- (length name) (length suffix)))
              path (path/join wiki-dir name)]]
    [id path]))

(defn acr-wiki-ids [wiki-dir]
  (as->
    (os/dir wiki-dir) .x
    (map |(remove-suffix $ ".acr") .x)))
