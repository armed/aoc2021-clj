(ns day13
  (:require [clojure.string :as string]
            [utils :as u]))

(defn parse-input
  [input-file-name]
  (let [[coords folds]
        (->> input-file-name
             (slurp)
             (string/split-lines)
             (split-with #(not= "" %)))]
    {:coords (transduce
              (comp
               (map #(string/split % #","))
               (map (fn [[a b]] [(u/parse-int a) (u/parse-int b)])))
              conj
              coords)
     :folds (transduce
             (comp
              (map #(subs % 11))
              (map #(string/split % #"="))
              (map (fn [[c v]]
                     [(keyword c) (u/parse-int v)])))
             conj
             (rest folds))}))

(def test-input (parse-input "src/day13_test_input.txt"))
(def day-input (parse-input "src/day13_input.txt"))

(defn coords->matrix
  [coords]
  (let [max-cols (->> coords (map first) (apply max) inc)
        max-rows (->> coords (map second) (apply max) inc)
        empty-matrix (vec (repeat max-rows (vec (repeat max-cols " "))))]
    (reduce (fn [matrix [x y]]
              (assoc-in matrix [y x] "#"))
            empty-matrix
            coords)))

(defn fold-y
  [num coords]
  (reduce (fn [acc [x y]]
            (let [y (if (> y num)
                      (- num (- y num))
                      y)]
              (conj acc [x y])))
          []
          coords))

(defn fold-x
  [num coords]
  (reduce (fn [acc [x y]]
            (let [x (if (> x num)
                      (- num (- x num))
                      x)]
              (conj acc [x y])))
          []
          coords))

(def fold-fns {:y fold-y :x fold-x})

(defn fold
  [[k v] coords]
  (let [fold-fn (get fold-fns k)]
    (set (fold-fn v coords))))

(defn calc-result-1
  [instruction coords]
  (count
   (fold instruction coords)))

(defn calc-result-2
  [{:keys [coords folds]}]
  (->> folds
       (reduce (fn [coords instruction]
                 (fold instruction coords))
               coords)
       (coords->matrix)
       (map println)))

(comment
 (calc-result-1 [:y 7] (:coords test-input))
 (calc-result-1 (-> day-input :folds first) (:coords day-input))

 (calc-result-2 test-input)
 (calc-result-2 day-input)

 " =>
[# # #     # # # #   #     #   # # #     #     #   # # #     #     #   # # #  ]
[#     #   #         #     #   #     #   #     #   #     #   #   #     #     #]
[#     #   # # #     #     #   #     #   #     #   #     #   # #       #     #]
[# # #     #         #     #   # # #     #     #   # # #     #   #     # # #  ]
[#   #     #         #     #   #         #     #   #         #   #     #   #  ]
[#     #   # # # #     # #     #           # #     #         #     #   #     #]
"
 )