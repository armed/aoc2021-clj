(ns day10
  (:require [clojure.string :as string]
            [utils :as u]))

(def closing? #{\) \} \] \>})

(def open-pairs {\) \(
                 \} \{
                 \] \[
                 \> \<})

(def close-pairs {\( \)
                  \{ \}
                  \[ \]
                  \< \>})

(def points {\) 3
             \] 57
             \} 1197
             \> 25137})

(def close-points {\) 1
                   \] 2
                   \} 3
                   \> 4})

(def test-input (as-> "src/day10_test_input.txt" n
                  (slurp n)
                  (string/split-lines n)))

(def day-input (as-> "src/day10_input.txt" n
                 (slurp n)
                 (string/split-lines n)))

(defn scan-line
  [line]
  (loop [c (first line)
         rest-of-string (rest line)
         open-chars []]
    (cond
      (nil? c) [open-chars nil]
      (and (closing? c) (not= (last open-chars) (get open-pairs c)))
      [open-chars c]

      :else
      (recur (first rest-of-string)
             (rest rest-of-string)
             (if (not (closing? c))
               (conj open-chars c)
               (into [] (butlast open-chars)))))))

(defn calc-task-1
  [input]
  (apply + (map (fn [line]
                  (let [[_ c] (scan-line line)]
                    (get points c 0)))
                input)))

(defn get-score
  [points]
  (reduce (fn [score point] (+ (* score 5) point)) 0 points))

(defn calc-task-2
  [input]
  (let [scores (->> input
                    (transduce
                     (comp
                      (map scan-line)
                      (filter (comp not second))
                      (map first)
                      (map #(map (partial get close-pairs) %))
                      (map reverse)
                      (map #(map (partial get close-points) %))
                      (map get-score))
                     conj)
                    (flatten)
                    (sort))]
    (-> (/ (dec (count scores)) 2)
        (drop scores)
        (first))))

(comment
 (calc-task-1 test-input)
 (calc-task-1 day-input)

 (calc-task-2 test-input)
 (calc-task-2 day-input)

 )
