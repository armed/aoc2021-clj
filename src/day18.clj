(ns day18
  (:require [clojure.zip :as z]
            [hyperfiddle.rcf :refer [tests]]
            [day18-input :refer [day-input]]))

(hyperfiddle.rcf/enable!)

(def test-input [[[[0 [5 8]] [[1 7] [9 6]]] [[4 [1 2]] [[1 4] 2]]]
                 [[[5 [2 8]] 4] [5 [[9 9] 0]]]
                 [6 [[[6 2] [5 6]] [[7 6] [4 7]]]]
                 [[[6 [0 7]] [0 9]] [4 [9 [9 0]]]]
                 [[[7 [6 4]] [3 [1 3]]] [[[5 5] 1] 9]]
                 [[6 [[7 3] [3 2]]] [[[3 8] [5 7]] 4]]
                 [[[[5 4] [7 7]] 8] [[8 3] 8]]
                 [[9 3] [[9 9] [6 [4 9]]]]
                 [[2 [[7 7] 7]] [[5 8] [[9 3] [0 2]]]]
                 [[[[5 2] 5] [8 [3 7]]] [[5 [7 5]] [4 4]]]])

(defn pair?
  [z]
  (let [v (z/node z)]
    (and (vector? v) (every? number? v))))

(defn explodable?
  [z]
  (and (pair? z) (>= (count (z/path z)) 4)))

(defn scalar?
  [z]
  (number? (z/node z)))

(defn split?
  [z]
  (let [v (z/node z)]
    (and (pos-int? v) (>= v 10))))

(defn find-node
  [loc direction pred]
  (cond
    (nil? loc) nil
    (z/end? loc) nil
    (pred loc) loc
    :else (recur (direction loc) direction pred)))

(defn find-scalar
  [loc direction]
  (find-node loc direction scalar?))

(defn find-explodable
  [loc]
  (find-node loc z/next explodable?))

(defn find-splittable
  [loc]
  (find-node loc z/next split?))

(tests
  (-> [[[[0 7] 4] [15 [0 13]]] [1 1]]
      (z/vector-zip)
      (find-splittable)
      (z/node))) := 15

(defn increase-scalar
  [loc v direction]
  (if-let [loc (find-scalar loc direction)]
    (z/edit loc + v)
    loc))

(defn source?
  [loc]
  (= :source (z/node loc)))

(defn explode
  [explodable]
  (let [[l r] (z/node explodable)]
    (-> (z/replace explodable :source)
        (increase-scalar l z/prev)
        (z/next)
        (increase-scalar r z/next)
        (find-node z/prev source?)
        (z/replace 0)
        (z/root))))

(defn split
  [loc]
  (let [v (/ (z/node loc) 2)
        left (int v)
        right (-> v Math/ceil int)]
    (-> loc
        (z/replace [left right])
        (z/root))))

(defn perform-cycle
  [snum]
  (when snum
    (let [zip (z/vector-zip snum)
          splitable (find-splittable zip)
          explodable (find-explodable zip)]
      (cond
        explodable (explode explodable)
        splitable (split splitable)
        :else nil))))

(defn add-snums
  [snum1 snum2]
  (loop [snum [snum1 snum2]]
    (if-let [new-snum (perform-cycle snum)]
      (recur new-snum)
      snum)))

(defn input-sum
  [input]
  (reduce add-snums input))

(defn magnitude-pair
  [loc]
  (when loc
    (z/edit loc (fn [[a b]] (+ (* a 3) (* b 2))))))

(defn magnitude
  [snum]
  (loop [loc (z/vector-zip snum)]
    (if-let [mg-pair (-> loc (find-node z/next pair?) (magnitude-pair))]
      (recur (-> mg-pair (z/root) (z/vector-zip)))
      (z/node loc))))

(defn calc-task-1
  [input]
  (-> input
      input-sum
      magnitude))

(defn calc-task-2
  [input]
  (let [[i1 i2] (split-at (/ (count input) 2) input)]
    (first
     (sort-by identity >
              (for [n1 i1
                    n2 i2
                    :let [m1 (-> (add-snums n1 n2) (magnitude))
                          m2 (-> (add-snums n2 n1) (magnitude))]]
                (max m1 m2))))))

(tests
  (calc-task-1 test-input) := 4140
  (calc-task-2 test-input) := 3993)

(comment
 (calc-task-1 day-input)
 (calc-task-2 day-input)
 )

(tests
  (perform-cycle [[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]])
  := [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]]

  (perform-cycle [[[[[9 8] 1] 2] 3] 4])
  := [[[[0 9] 2] 3] 4]

  (perform-cycle [7 [6 [5 [4 [3 2]]]]])
  := [7 [6 [5 [7 0]]]]

  (perform-cycle [[6 [5 [4 [3 2]]]] 1])
  := [[6 [5 [7 0]]] 3]

  (perform-cycle [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]])
  := [[3 [2 [8 0]]] [9 [5 [7 0]]]]

  (perform-cycle [[[[[4 3] 4] 4] [7 [[8 4] 9]]] [1 1]])
  := [[[[0 7] 4] [7 [[8 4] 9]]] [1 1]]

  (perform-cycle [[[[0 7] 4] [7 [[8 4] 9]]] [1 1]])
  := [[[[0 7] 4] [15 [0 13]]] [1 1]]

  (perform-cycle [[[[0 7] 4] [15 [0 13]]] [1 1]])
  := [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]

  (perform-cycle [[[[0 7] 4] [[7 8] [0 13]]] [1 1]])
  := [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]]

  (add-snums [[[0 [4 5]] [0 0]] [[[4 5] [2 6]] [9 5]]]
             [7 [[[3 7] [4 3]] [[6 3] [8 8]]]])
  := [[[[4 0] [5 4]] [[7 7] [6 0]]] [[8 [7 7]] [[7 9] [5 0]]]]

  (add-snums [[[[4 0] [5 4]] [[7 7] [6 0]]] [[8 [7 7]] [[7 9] [5 0]]]]
             [[2 [[0 8] [3 4]]] [[[6 7] 1] [7 [1 6]]]])
  := [[[[6 7] [6 7]] [[7 7] [0 7]]] [[[8 7] [7 7]] [[8 8] [8 0]]]]

  (add-snums [[[[6 7] [6 7]] [[7 7] [0 7]]] [[[8 7] [7 7]] [[8 8] [8 0]]]]
             [[[[2 4] 7] [6 [0 5]]] [[[6 8] [2 8]] [[2 1] [4 5]]]])
  := [[[[7 0] [7 7]] [[7 7] [7 8]]] [[[7 7] [8 8]] [[7 7] [8 7]]]]

  (add-snums [[[[7 0] [7 7]] [[7 7] [7 8]]] [[[7 7] [8 8]] [[7 7] [8 7]]]]
             [7 [5 [[3 8] [1 4]]]])
  := [[[[7 7] [7 8]] [[9 5] [8 7]]] [[[6 8] [0 8]] [[9 9] [9 0]]]]

  (add-snums [[[[7 7] [7 8]] [[9 5] [8 7]]] [[[6 8] [0 8]] [[9 9] [9 0]]]]
             [[2 [2 2]] [8 [8 1]]])
  := [[[[6 6] [6 6]] [[6 0] [6 7]]] [[[7 7] [8 9]] [8 [8 1]]]]

  (add-snums [[[[6 6] [6 6]] [[6 0] [6 7]]] [[[7 7] [8 9]] [8 [8 1]]]]
             [2 9])
  := [[[[6 6] [7 7]] [[0 7] [7 7]]] [[[5 5] [5 6]] 9]]

  (add-snums [[[[6 6] [7 7]] [[0 7] [7 7]]] [[[5 5] [5 6]] 9]]
             [1 [[[9 3] 9] [[9 0] [0 7]]]])
  := [[[[7 8] [6 7]] [[6 8] [0 8]]] [[[7 7] [5 0]] [[5 5] [5 6]]]]

  (add-snums [[[[7 8] [6 7]] [[6 8] [0 8]]] [[[7 7] [5 0]] [[5 5] [5 6]]]]
             [[[5 [7 4]] 7] 1])
  := [[[[7 7] [7 7]] [[8 7] [8 7]]] [[[7 0] [7 7]] 9]]

  (add-snums [[[[7 7] [7 7]] [[8 7] [8 7]]] [[[7 0] [7 7]] 9]]
             [[[[4 2] 2] 6] [8 7]])
  := [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]]

  (magnitude [[1 2] [[3 4] 5]]) := 143
  (magnitude [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]) := 1384
  (magnitude [[[[1 1] [2 2]] [3 3]] [4 4]]) := 445
  (magnitude [[[[3 0] [5 3]] [4 4]] [5 5]]) := 791
  (magnitude [[[[5 0] [7 4]] [5 5]] [6 6]]) := 1137
  (magnitude [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]]) := 3488

  (-> test-input (input-sum) (magnitude)) := 4140
  )
