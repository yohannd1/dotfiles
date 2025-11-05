(use dotf-path)

(defn acr-read-header [fd]
  (var running true)
  (def lines-iter (file/lines fd))
  (def ret @{})

  (defn rec []
    (when (def line-k (next lines-iter))
      (def line (in lines-iter line-k))
      (if (string/has-prefix? "%:" line)
        (let [[start v] (string/split " " line 0 2)
              k (-> start (string/slice 2) (keyword))]
          (set (ret k) (string/trim v))
          (rec)))))

  (rec)
  ret)

(defn acr-slurp-header [path]
  (with [fd (file/open path :rn)]
    (acr-read-header fd)))

(defn acr-get-wiki-note-pairs [wiki-dir]
  (seq [name :in (os/dir wiki-dir)
        :let [suffix ".acr"]
        :when (string/has-suffix? suffix name)
        :let [id (string/slice name 0 (- (length name) (length suffix)))
              path (path/join wiki-dir name)]]
    [id path]))
