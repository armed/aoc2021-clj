(ns day15
  (:require [clojure.string :as string]
            [utils :as u]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse-input
  [input-file-name]
  (->> input-file-name
       (slurp)
       (string/split-lines)
       (mapv #(mapv u/parse-int %))))

(def test-input (parse-input "src/day15_test_input.txt"))
(def test-input-2 (parse-input "src/day15_test_input_2.txt"))
(def day-input (parse-input "src/day15_input.txt"))

(defn exists?
  [graph [row col]]
  (and (>= row 0)
       (>= col 0)
       (< row (count graph))
       (< col (count (first graph)))))

(defn find-neighbours
  [[row col]]
  [[(dec row) col]
   [(inc row) col]
   [row (dec col)]
   [row (inc col)]])

(defn scan-neighbours
  [graph current node-links risk-levels]
  (loop [candidates (priority-map)
         nx (->> current (find-neighbours) (filter #(exists? graph %)))
         risks risk-levels
         links node-links]
    (if-let [n (first nx)]
      (let [cost (get risks current)
            new-cost (+ cost (get-in graph current))
            nx (rest nx)]
        (if (< new-cost (get risks n Integer/MAX_VALUE))
          (let [priority new-cost]
            (recur (assoc candidates n priority)
                   nx
                   (assoc risks n new-cost)
                   (assoc links n current)))
          (recur candidates nx risks links)))
      [candidates links risks])))

(defn find-path
  [graph]
  (let [start [0 0]
        finish [(dec (count graph))
                (dec (-> graph first count))]]
    (loop [candidates (priority-map start 0)
           node-links {start nil}
           risk-levels {start 0}]
      (if-let [current (ffirst candidates)]
        (if (= finish current)
          node-links
          (let [[nbs risks links] (scan-neighbours graph
                                                   current
                                                   node-links
                                                   risk-levels)]
            (recur (conj (pop candidates) nbs) risks links)))
        node-links))))

(defn find-minimum-risk
  [input]
  (let [size (dec (count input))
        node-links (find-path input)
        path (loop [current [size size]
                    path []]
               (if (= [0 0] current)
                 (reverse path)
                 (recur (get node-links current) (conj path current))))]
    (reduce (fn [acc step]
              (+ acc (get-in input step)))
            0
            path)))

(defn new-val
  [input r c]
  (let [size (count input)
        rx (int (/ r size))
        cx (int (/ c size))
        row (rem r size)
        col (rem c size)
        v (get-in input [row col])
        n (+ v rx cx)]
    (if (> n 9)
      (rem n 9)
      n)))

(defn extend-row
  [input r max-col]
  (let [size (count input)]
    (for [col (range max-col)
          :when (or (>= col size)
                    (>= r size))]
      (new-val input r col))))

(defn extend-map
  [input times]
  (let [size (count input)
        new-size (* times size)]
    (reduce (fn [m r]
              (update m r into (extend-row input r new-size)))
            (into input (repeat (- new-size size) []))
            (range new-size))))

(comment
 (find-minimum-risk test-input)
 (find-minimum-risk test-input-2)

 (= test-input-2
    (extend-map test-input 5))

 (find-minimum-risk day-input)
 (time
  (find-minimum-risk (extend-map day-input 5)))

 )