(ns day2
  (:require [clojure.string :as string]
            [utils :as u]))

(defn parse-command-and-value
  [s]
  (let [[c v] (string/split s #"\s")]
    (let [direction (keyword c)
          amount (u/parse-int v)
          up? (= :up direction)]
      (if up?
        [:down (- amount)]
        [direction amount]))))

(def day-input
  (as-> "src/day2_input.txt" n
    (slurp n)
    (string/split n #"\n")
    (map parse-command-and-value n)))

(defn inc-by
  [s v]
  (+ s v))

(defn calc-result-1
  [day-input]
  (->> day-input
       (reduce (fn [acc [op val]]
                 (update acc op inc-by val))
               {:down 0 :forward 0})
       vals
       (apply *)))

(defn calc-result-2
  [day-input]
  (->> day-input
       (reduce (fn [[down forward aim] [op val]]
                 (let [f? (= :forward op)]
                   [(if f? (+ down (* aim val)) down)
                    (if f? (+ forward val) forward)
                    (if (not f?) (inc-by aim val) aim)]))
               [0 0 0])
       (take 2)
       (apply *)))

(comment
 (calc-result-1 day-input)
 (calc-result-2 day-input)
 )
