(ns day6
  (:require [clojure.string :as string]
            [utils :as u]))

(def test-input [3 4 3 1 2])

(def day-input
  (as-> "src/day6_input.txt" n
    (slurp n)
    (string/split n #",")
    (map u/parse-int n)))

(defn calc-result
  [input generation-count]
  (loop [steps-left generation-count
         freqs (frequencies input)]
    (if (zero? steps-left)
      (->> freqs vals (apply +))
      (recur
       (dec steps-left)
       (reduce (fn [acc gen]
                 (if-let [v (get freqs gen)]
                   (if (zero? gen)
                     (merge acc {6 v 8 v})
                     (update acc (dec gen) #(+ (or % 0) v)))
                   acc))
               {}
               (range 0 9))))))

(comment
 (calc-result day-input 256))