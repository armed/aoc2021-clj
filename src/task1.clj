(ns task1
  (:require [clojure.string :as string]))

(def task-data
  (as-> "src/task1_input.txt" n
    (slurp n)
    (string/split n #"\s")
    (map #(Integer/parseInt %) n)))

(defn task1-result
  []
  (first
   (reduce (fn [[cnt prev] cur]
             [(if (> cur prev) (inc cnt) cnt)
              cur])
           [-1 0]
           task-data)))

(defn task2-result
  []
  (loop [td task-data
         cnt 0]
    (let [w1 (take 3 td)
          w2 (take 3 (drop 1 td))]
      (if (< (+ (count w1) (count w2)) 6)
        cnt
        (let [s1 (apply + w1)
              s2 (apply + w2)]
          (recur
           (drop 1 td)
           (if (> s2 s1) (inc cnt) cnt)))))))

(comment
 (task1-result)
 (task2-result)
  )