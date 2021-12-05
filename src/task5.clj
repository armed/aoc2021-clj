(ns task5
  (:require [clojure.string :as string]
            [utils :as u]))

(defn straight?
  [[[x1 y1] [x2 y2]]]
  (or (= x1 x2) (= y1 y2)))

(def straight-lines-filter
  (filter straight?))

(defn straight-or-diagonal45?
  [[[x1 y1] [x2 y2] :as pairs]]
  (or (straight? pairs)
      (= (u/abs (- x1 x2))
         (u/abs (- y1 y2)))))

(def diagonal-45-allowed-filter
  (filter straight-or-diagonal45?))

(defn get-task-data
  [line-filter]
  (->> "src/task5_input.txt"
       (slurp)
       (string/split-lines)
       (transduce
        (comp
         (map #(string/split % #" -> "))
         (map (fn [from->to]
                (map
                 #(as-> % n
                    (string/split n #",")
                    (map u/parse-int n))
                 from->to)))
         line-filter)
        conj)))

(defn coord-range
  [pairs-count c1 c2]
  (if (= c1 c2)
    (repeat pairs-count c1)
    (let [step (if (> c2 c1) 1 -1)]
      (range c1 (+ c2 step) step))))

(defn calc-result
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

(comment
 ;; result 1
 (calc-result (get-task-data straight-lines-filter))
 ;; result 2
 (calc-result (get-task-data diagonal-45-allowed-filter))

 )



