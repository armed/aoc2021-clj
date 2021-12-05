(ns task1
  (:require [clojure.string :as string]
            [utils :as u]))

(def task-data
  (as-> "src/task1_input.txt" n
    (slurp n)
    (string/split n #"\s")
    (map u/parse-int n)))

(defn task1-result
  []
  #_ "better solution (seen from tonsky)"
  #_ (->> (next task-data)
          (map - task-data)
          (filter neg?)
          (count))
  #_ "my abomination"
  (first
   (reduce (fn [[cnt prev] cur]
             [(if (> cur prev) (inc cnt) cnt)
              cur])
           [-1 0]
           task-data)))

(defn task2-result
  []
  #_ "better solutin (like 1)"
  #_ "forgot that partition have step param"
  #_ (->>
      (map (fn [w1 w2]
             (- (apply + w1) (apply + w2)))
           (partition 3 1 task-data)
           (partition 3 1 (next task-data)))
      (filter neg?)
      count)
  #_ "my abomination"
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