(use dotf-utils)
(use dotf-path)

(defn acr-parse-file [fd &named only-header]
  (def h-entries @{})
  (def ret @{:header h-entries})

  (def lines-iter (file/lines fd))
  (var last-lnum 0)
  (defn next-line []
    (def k (next lines-iter))
    (unless (nil? k)
      (++ last-lnum)
      (in lines-iter k)))

  (letrec rec []
    (when (def line (next-line))
      (if (string/has-prefix? "%:" line)
        (do
          (def [start ev] (string/split " " line 0 2))
          (def ek (-> start (string/slice 2) keyword))
          (set (h-entries ek) (string/trim ev))
          (rec))
        (set (ret :body-start-lnum) last-lnum))))

  (when only-header
    (break ret))

  # push everything in the body into a buffer (TODO: should I instead split it into lines already?)
  (def body @"")
  (while (def line (next-line))
    (buffer/push-string body line)
    (each line lines-iter
      (buffer/push-string body line)))
  (set (ret :body) body)

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
