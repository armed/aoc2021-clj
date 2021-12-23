(ns day1
  (:require [clojure.string :as string]
            [utils :as u]))

(def day-input
  (as-> "src/day1_input.txt" n
    (slurp n)
    (string/split n #"\s")
    (map u/parse-int n)))

(defn task1-result
  []
  #_"better solution (seen from tonsky)"
  #_(->> (next day-input)
         (map - day-input)
         (filter neg?)
         (count))
  #_"my abomination"
  (first
   (reduce (fn [[cnt prev] cur]
             [(if (> cur prev) (inc cnt) cnt)
              cur])
           [-1 0]
           day-input)))

(defn task2-result
  []
  #_"better solutin (tonsky)"
  #_"forgot that partition has step param"
  #_(->>
     (map (fn [w1 w2]
            (- (apply + w1) (apply + w2)))
          (partition 3 1 day-input)
          (partition 3 1 (next day-input)))
     (filter neg?)
     count)
  #_"my abomination"
  (loop [td day-input
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