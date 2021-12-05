(ns utils)

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn abs
  [v]
  (Math/abs ^long v))