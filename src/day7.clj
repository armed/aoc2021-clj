(ns day7
  (:require [clojure.string :as string]
            [utils :as u]))

(def test-input [16,1,2,0,4,2,7,1,2,14])

(def day-input
  (as-> "src/day7_input.txt" n
    (slurp n)
    (string/split n #",")
    (map u/parse-int n)))

(defn calc-task
  ([input]
   (calc-task input identity))
  ([input fuel-fn]
   (as-> (sort input) n
     (range (first n) (inc (last n)))
     (reduce (fn [fuel destination]
               (->> input
                    (reduce
                     (fn [acc position]
                       (-> position
                           (- destination)
                           (u/abs)
                           (fuel-fn)
                           (+ acc)))
                     0)
                    (min fuel)))
             Integer/MAX_VALUE
             n))))

(defn calc-task-1
  [input]
  (calc-task input))

(defn calc-fuel
  [distance]
  (/ (* (+ 1 distance) distance) 2))

(defn calc-task-2
  [input]
  (calc-task input calc-fuel))

(comment
 (calc-task-1 test-input)
 (calc-task-1 day-input)

 (calc-task-2 test-input)
 (calc-task-2 day-input)
 
 )