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
      (and (closing? c) (not= (last open-chars)
                              (get open-pairs c)))
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
  (prn points)
  (reduce (fn [score point]
            (+ (* score 5) point))
          0
          points))

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
                      (map string/join)
                      (map #(map (partial get close-points) %))
                      (map get-score))
                     conj)
                    (flatten))]
    (->> scores
         (drop (count scores)))))

(comment
 (map #(map (partial get close-points) %) [(seq "])}>")])
 (get-score '(2 1 3 4))
 (filter (fn [line]
           (nil? (scan-line line)))
         test-input)

 (keep (comp not scan-line) test-input)
 (calc-task-1 test-input)
 (calc-task-1 day-input)

 (calc-task-2 test-input)

 (scan-line "<{([{{}}[<[[[<>{}]]]>[]]")

 (map (comp first scan-line) test-input)
 )
