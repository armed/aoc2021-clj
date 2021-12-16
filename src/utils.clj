(ns utils)

(defn parse-int
  ([s]
   (parse-int (str s) 10))
  ([s base]
   (Long/parseLong (str s) base)))

(defn abs
  [v]
  (Math/abs ^long v))