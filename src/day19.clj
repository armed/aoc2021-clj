(ns day19
  (:require [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as string]
            [utils :as u]))

(hyperfiddle.rcf/enable!)

(defn parse-line
  [line]
  (->> line
       (re-seq #"(-{0,1}\d+)+")
       (mapv (comp u/parse-int second))))

(defn parse-input
  [file-name]
  (->> file-name
       (slurp)
       (string/split-lines)
       (reduce (fn [data line]
                 (cond
                   (string/starts-with? line "---")
                   (conj data [])

                   (empty? line) data

                   :else (->> line
                              parse-line
                              (update data (dec (count data)) conj))))
               [])))

(def test-input (parse-input "src/day19_test_input.txt"))
(def day-input (parse-input "src/day19_input.txt"))

(def orientations
  [{:r [0 1 2], :signs [1 1 1]}
   {:r [1 0 2], :signs [-1 1 1]}
   {:r [0 1 2], :signs [-1 -1 1]}
   {:r [1 0 2], :signs [1 -1 1]}
   {:r [2 1 0], :signs [-1 1 1]}
   {:r [1 2 0], :signs [-1 -1 1]}
   {:r [2 1 0], :signs [1 -1 1]}
   {:r [1 2 0], :signs [1 1 1]}
   {:r [0 1 2], :signs [-1 1 -1]}
   {:r [1 0 2], :signs [-1 -1 -1]}
   {:r [0 1 2], :signs [1 -1 -1]}
   {:r [1 0 2], :signs [1 1 -1]}
   {:r [2 1 0], :signs [1 1 -1]}
   {:r [1 2 0], :signs [-1 1 -1]}
   {:r [2 1 0], :signs [-1 -1 -1]}
   {:r [1 2 0], :signs [1 -1 -1]}
   {:r [0 2 1], :signs [1 -1 1]}
   {:r [2 0 1], :signs [1 1 1]}
   {:r [0 2 1], :signs [-1 1 1]}
   {:r [2 0 1], :signs [-1 -1 1]}
   {:r [0 2 1], :signs [1 1 -1]}
   {:r [2 0 1], :signs [-1 1 -1]}
   {:r [0 2 1], :signs [-1 -1 -1]}
   {:r [2 0 1], :signs [1 -1 -1]}])

(defn translate
  [{:keys [r signs]} coord]
  (mapv *
        (map (fn [idx] (nth coord idx)) r)
        signs))

(defn variants
  [coord]
  (for [{:keys [r signs] :as o} orientations]
    {:r r
     :coord (translate o coord)
     :signs signs}))

(variants [1 2 3])

(defn overlap-data
  [scanner-a scanner-b]
  (let [s2variants (mapcat variants scanner-b)]
    (mapcat (fn [[x y z]]
              (map (fn [{:keys [r coord signs]}]
                     (let [[x' y' z'] coord]
                       {:r r
                        :signs signs
                        :coord [(- x x') (- y y') (- z z')]}))
                   s2variants))
            scanner-a)))

(defn find-overlap*
  [idx1 idx2 input]
  (let [scanner-a (nth input idx1)
        scanner-b (nth input idx2)]
    (->> (overlap-data scanner-a scanner-b)
         (frequencies)
         (filter (fn [[_ c]] (>= c 12)))
         (ffirst))))

(defonce find-overlap (memoize find-overlap*))

(defn delta-converter
  [{:keys [coord] :as delta}]
  (when delta
    (let [[dx dy dz] coord]
      (fn [source-coord]
        (let [[x y z] (translate delta source-coord)]
          [(+ dx x) (+ dy y) (+ dz z)])))))

(defn find-all-overlaps
  [input]
  (let [overlaps (for [idx1 (range (count input))
                       idx2 (range (count input))
                       :let [overlap
                             (when (and (not= idx1 idx2) (> idx2 0))
                               (find-overlap idx1 idx2 input))]
                       :when overlap]
                   [idx1 (assoc overlap :idx idx2)])]
    (reduce (fn [acc [k v]]
              (update acc k assoc (:idx v) v))
            {}
            overlaps)))

(defn build-map
  ([input overlaps]
   (build-map input overlaps 0 #{0}))
  ([input overlaps idx visits]
   (let [ov-list (->> idx
                      (get overlaps)
                      (filter #(not (contains? visits (first %)))))
         visits' (into visits (map first ov-list))]
     (reduce (fn [acc [i ov]]
               (into acc
                     (map (delta-converter ov))
                     (build-map input overlaps i visits')))
             (set (get input idx))
             ov-list))))

(defn sensor-coord-list
  ([overlaps idx]
   (sensor-coord-list overlaps idx 0 #{0}))
  ([overlaps idx parent-idx visits]
   (if-let [ov (get-in overlaps [parent-idx idx])]
     ((delta-converter ov) [0 0 0])
     (let [ov-list (->> parent-idx
                        (get overlaps)
                        (filter #(not (contains? visits (first %)))))
           visits' (into visits (map first ov-list))]
       (some identity
             (for [[i ov] ov-list
                   :let [coord (sensor-coord-list overlaps idx i visits')]
                   :when coord]
               ((delta-converter ov) coord)))))))

(defn calc-task-1
  [input]
  (let [ovs (find-all-overlaps input)]
    (count (build-map input ovs))))

(defn calc-task-2
  [input]
  (let [ovs (find-all-overlaps input)
        coords (for [idx (range 1 (count input))]
                 {:idx idx
                  :coord (sensor-coord-list ovs idx)})]
    (reduce max
            (for [a coords
                  b coords
                  :when (and (not= (:idx a) (:idx b)))]
              (->> (map - (:coord a) (:coord b))
                   (map u/abs)
                   (reduce +))))))

(tests
  (calc-task-1 test-input) := 79
  (calc-task-2 test-input) := 3621

  (calc-task-1 day-input) := 408
  (calc-task-2 day-input) := 13348)