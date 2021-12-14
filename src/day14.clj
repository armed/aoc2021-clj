(ns day14
  (:require [clojure.string :as string]))

(defn parse-input
  [input-file-name]
  (let [[template _ & rules] (-> input-file-name
                                 (slurp)
                                 (string/split-lines))]
    {:pairs (->> template
                 (partition 2 1)
                 (map string/join)
                 (frequencies))
     :first-letter (first template)
     :rules (into {} (map #(string/split % #"\s->\s") rules))}))

(def test-input (parse-input "src/day14_test_input.txt"))
(def day-input (parse-input "src/day14_input.txt"))

(defn make-incrementer
  [amount]
  (fn [v]
    (+ (or v 0) amount)))

(defn perform-step
  [rules pairs]
  (let [new-pairs
        (reduce (fn [acc [[a b :as pair] amount]]
                  (let [incrementer (make-incrementer amount)
                        l (get rules pair)]
                    (-> acc
                        (update (str a l) incrementer)
                        (update (str l b) incrementer))))
                {}
                pairs)]
    new-pairs))

(defn count-letters
  [first-letter pairs]
  (reduce (fn [acc pair]
            (let [[_ a] pair
                  cnt (get pairs pair)]
              (assoc acc a (+ (get acc a 0) cnt))))
          {first-letter 1}
          (keys pairs)))

(defn calc-task
  [{:keys [pairs first-letter rules]} times]
  (let [counts (->> times
                    (nth (iterate #(perform-step rules %) pairs))
                    (count-letters first-letter)
                    (vals)
                    (sort))]
    (- (last counts) (first counts))))

(comment
 (calc-task test-input 10)
 (calc-task test-input 40)

 (calc-task day-input 10)
 (calc-task day-input 40)
 )
