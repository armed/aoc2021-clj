(ns utils)

(defn parse-int
  ([s]
   (parse-int (str s) 10))
  ([s base]
   (Integer/parseInt (str s) base)))

(defn abs
  [v]
  (Math/abs ^long v))