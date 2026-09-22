(defn print-table [fd tbl &named sep]
  (default sep " ")

  (when (empty? tbl)
    (break))

  (def nrows (length tbl))
  (def ncols (length (first tbl)))

  (each row tbl
    (def row-ncols (length row))
    (assert (= row-ncols ncols)
            (string "expected " ncols " columns, got " row-ncols)))

  (def max-lengths
    (seq [i :range [0 ncols]]
      (max ;(map |(-> (in $ i) (length)) tbl))))

  (defn wr-repeat [n x]
    (for _ 0 n (:write fd x)))

  (each row tbl
    (eachp [i c] row
      (when (> i 0) (:write fd sep))
      (:write fd c)
      (def to-pad (- (max-lengths i) (length c)))
      (wr-repeat to-pad " "))
    (:write fd "\n")))
