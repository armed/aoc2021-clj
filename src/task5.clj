(ns task5
  (:require [clojure.string :as string]
            [utils :as u]))

(defn straight?
  [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(defn straight-or-diagonal45?
  [[[x1 y1] [x2 y2] :as pairs]]
  (or (straight? pairs)
      (= (u/abs (- x1 x2))
         (u/abs (- y1 y2)))))

(def straight-lines-filter
  (partial filter straight?))

(def straight-or-diagonal45-filter
  (partial filter straight-or-diagonal45?))

(def measurement-data
  (->> "src/task5_input.txt"
       (slurp)
       (string/split-lines)
       (map (comp
             (partial partition 2)
             (partial map u/parse-int)
             rest
             (partial re-matches #"(\d+),(\d+)\s->\s(\d+),(\d+)")))))

(defn coord-range
  [pairs-count c1 c2]
  (if (= c1 c2)
    (repeat pairs-count c1)
    (let [step (if (> c2 c1) 1 -1)]
      (range c1 (+ c2 step) step))))

(defn calc-result-using-hashmap
  [task-data]
  (->> task-data
       (reduce (fn [diagram-data [[x1 y1] [x2 y2]]]
                 (let [pairs-count (+ 1 (max (u/abs (- x1 x2))
                                             (u/abs (- y1 y2))))]
                   (->> (map (fn [x y] {(str x "," y) 1})
                             (coord-range pairs-count x1 x2)
                             (coord-range pairs-count y1 y2))
                        (into {})
                        (merge-with + diagram-data))))
               {})
       vals
       (filter #(> % 1))
       count))

(defn calc-result-using-frequencies
  [task-data]
  (->> task-data
       (reduce (fn [diagram-data [[x1 y1] [x2 y2]]]
                 (let [pairs-count (+ 1 (max (u/abs (- x1 x2))
                                             (u/abs (- y1 y2))))]
                   (->> (map vector
                             (coord-range pairs-count x1 x2)
                             (coord-range pairs-count y1 y2))
                        (frequencies)
                        (merge-with + diagram-data))))
               {})
       vals
       (filter #(> % 1))
       count))

(comment
 ;; result 1
 (time
  (calc-result-using-hashmap
   (straight-lines-filter measurement-data)))

 (time
  (calc-result-using-frequencies
   (straight-lines-filter measurement-data)))

 ;; result 2
 (time
  (calc-result-using-hashmap
   (straight-or-diagonal45-filter measurement-data)))

 (time
  (calc-result-using-frequencies
   (straight-or-diagonal45-filter measurement-data)))
 )



