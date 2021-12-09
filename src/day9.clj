(ns day9
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [utils :as u]))

(def test-input [[2 1 9 9 9 4 3 2 1 0]
                 [3 9 8 7 8 9 4 9 2 1]
                 [9 8 5 6 7 8 9 8 9 2]
                 [8 7 6 7 8 9 6 7 8 9]
                 [9 8 9 9 9 6 5 6 7 8]])

(def day-input
  (as-> "src/day9_input.txt" n
    (slurp n)
    (string/split-lines n)
    (map #(string/split % #"") n)
    (mapv (fn [nums]
            (mapv u/parse-int nums))
          n)))

(def INFINITY 100)

(defn get-adj-coords
  [r c]
  (set
   (filter
    (fn [[r c]]
      (and (>= r 0) (>= c 0)))
    [[(dec r) c]
     [(inc r) c]
     [r (dec c)]
     [r (inc c)]])))

(defn get-lowest-level-data
  [heatmap r c]
  (let [self (get-in heatmap [r c])
        coords (get-adj-coords r c)
        adjacents (map #(get-in heatmap % INFINITY) coords)]
    (when (every? (partial < self) adjacents)
      [self coords])))

(defn find-levels
  [heatmap]
  (reduce (fn [{:keys [ignored risk-level lowest-levels] :as acc} [r c]]
            (if (contains? ignored [r c])
              acc
              (if-let [[level coords] (get-lowest-level-data heatmap r c)]
                {:risk-level (+ risk-level level 1)
                 :lowest-levels (conj lowest-levels [r c])
                 :ignored (set/union ignored (set coords))}
                acc)))
          {:ignored #{}
           :lowest-levels #{}
           :risk-level 0}
          (for [r (range (count heatmap))
                c (range (count (first heatmap)))]
            [r c])))

(defn calc-task-1
  [heatmap]
  (let [result (find-levels heatmap)]
    (:risk-level result)))

(defn scan-basin
  ([heatmap row col]
   (second (scan-basin heatmap row col #{})))
  ([heatmap row col visited]
   (let [current-visited (conj visited [row col])
         level (get-in heatmap [row col])
         coords [row col]]
     (if (and level (> 9 level))
       (if-let [adj (seq (set/difference (get-adj-coords row col) visited))]
         (reduce (fn [[vis crds] [r c]]
                   (let [[v c] (scan-basin heatmap r c vis)]
                     [(set/union vis v) (into crds c)]))
                 [current-visited #{coords}]
                 adj)
         [current-visited #{coords}])
       [current-visited #{}]))))

(defn calc-task-2
  [heatmap]
  (as-> heatmap n
    (find-levels n)
    (:lowest-levels n)
    (map (fn [[r c]] (scan-basin heatmap r c)) n)
    (sort-by count n)
    (reverse n)
    (take 3 n)
    (map count n)
    (apply * n)))

(comment
 (calc-task-1 test-input)
 (calc-task-2 test-input)

 (calc-task-1 day-input)
 (calc-task-2 day-input))