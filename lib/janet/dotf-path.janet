(defn path/basename
  "Get the base name (the last component) of a path."
  [path]

  (as-> path .x (string/split "/" .x) (last .x)))

(defn path/is-dir?
  "Checks whether a path is a directory."
  [path]

  (if-let [data (os/stat path)]
    (-> data (in :mode) (= :directory))
    false
    )
  )

(defn path/extension
  "Get the extension of a path."
  [path]

  (as->
    path .x
    (path/basename .x)
    (string/split "." .x)
    (if (-> .x (length) (<= 1))
      nil
      (last .x))
    ))

(defn path/exists?
  "Checks whether a path exists."
  [path]

  (-> (os/stat path) (nil?) (not)))

(defn path/absolute [path]
  (if (string/has-prefix? "/" path)
    path
    (string (os/cwd) "/" path)))

(defn path/dirname [path]
  (when (empty? path)
    (break "."))

  (var i (-> path (length) (- 1)))
  (def chr-slash (chr "/"))
  (while (and (> i 0) (-> (in path i) (not= chr-slash)))
    (-- i))

  (if (= i 0) "." (string/slice path 0 i)))

(defn path/join [path x]
  (if (= path "") x (string path "/" x)))

(assert (= (path/dirname "foo/bar") "foo"))
(assert (= (path/dirname "foo") "."))
(assert (= (path/join "" "hello") "hello"))
(assert (= (path/join "/foo/bar" "hello") "/foo/bar/hello"))
(assert (= (path/absolute "/") "/"))
