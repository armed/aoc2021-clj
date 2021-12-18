(ns day18
  (:require [clojure.zip :as z]
            [hyperfiddle.rcf :refer [tests]]))

(hyperfiddle.rcf/enable!)

(defn pair?
  [v]
  (and (vector? v)
       (= 2 (count v))
       (every? pos-int? v)))

(defn find-scalar
  [loc direction]
  (cond
    (nil? loc) nil
    (z/end? loc) nil
    (pos-int? (z/node loc)) loc
    :else (recur (direction loc) direction)))

(defn find-pair
  [loc]
  (cond
    (z/end? loc) nil
    (pair? (z/node loc)) loc
    :else (recur (z/next loc))))

(defn try-increase-scalar
  [loc v direction]
  (if-let [loc (find-scalar loc direction)]
    (z/edit loc + v)
    loc))

(defn explode
  [loc]
  (let [pair (find-pair (z/vector-zip loc))
        [l r] (z/node pair)
        summed (some-> (z/replace pair 0)
                       (try-increase-scalar l z/prev)
                       (z/next)
                       (try-increase-scalar r z/next))]
    (z/root summed)))

(tests
  (explode [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]])
  := [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]]

  (explode [[[[[9, 8], 1], 2], 3], 4])
  := [[[[0, 9], 2], 3], 4]

  (explode [7, [6, [5, [4, [3, 2]]]]])
  := [7, [6, [5, [7, 0]]]]

  (explode [[6, [5, [4, [3, 2]]]], 1])
  := [[6, [5, [7, 0]]], 3]

  (explode [[3, [2, [8, 0]]], [9, [5, [4, [3, 2]]]]])
  := [[3, [2, [8, 0]]], [9, [5, [7, 0]]]]
  )

(comment
 (explode [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]])

 (-> [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]]
     (z/vector-zip)
     (find-pair)
     (z/replace 0)
     (find-scalar z/prev)
     (z/edit + 7)
     (z/next)
     (find-scalar z/next)
     (z/edit + 3)
     (z/root)
     ;(find-scalar z/next)
     ;(drop-while not-scalar?)
     ;first
     ;first
     ;(z/replace 0)
     ;(iterate z/prev)
     ;(drop-while not-scalar?)
     ;first
     ;(take-while pair?)
     ;(z/node)
     ;(take-while pair?)
     ;(find-nearest-left-node)
     ;(find-nearest-right-node)
     ;(find-nearest-left-number)
     ;(find-nearest-left-number)
     ;(find-nearest-right-number)
     )

 )
