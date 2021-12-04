(ns task2
  (:require [clojure.string :as string]))

(defn parse-command-and-value
  [s]
  (let [[c v] (string/split s #"\s")]
    (let [direction (keyword c)
          amount (Integer/parseInt v)
          up? (= :up direction)]
      (if up?
        [:down (- amount)]
        [direction amount]))))

(def task-data
  (as-> "src/task2_input.txt" n
    (slurp n)
    (string/split n #"\n")
    (map parse-command-and-value n)))

(defn inc-by
  [s v]
  (+ s v))

(defn calc-result-1
  [task-data]
  (->> task-data
       (reduce (fn [acc [op val]]
                 (update acc op inc-by val))
               {:down 0 :forward 0})
       vals
       (apply *)))

(defn calc-result-2
  [task-data]
  (->> task-data
       (reduce (fn [[down forward aim] [op val]]
                 (let [f? (= :forward op)]
                   [(if f? (+ down (* aim val)) down)
                    (if f? (+ forward val) forward)
                    (if (not f?) (inc-by aim val) aim)]))
               [0 0 0])
       (take 2)
       (apply *)))

(comment
 (calc-result-1 task-data)
 (calc-result-2 task-data)
 )
