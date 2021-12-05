(ns utils)

(defn parse-int
  ([s]
   (parse-int s 10))
  ([s base]
   (Integer/parseInt s base)))

(defn abs
  [v]
  (Math/abs ^long v))