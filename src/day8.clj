(ns day8
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [utils :as u]))

"
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
"

(defn parse-input
  [file-name]
  (as-> (slurp file-name) n
    (string/split-lines n)
    (map (fn [l]
           (split-at 10 (map second (re-seq #"([a-g]+)+" l)))) n)))

(def day-input (parse-input "src/day8_input.txt"))
(def test-input (parse-input "src/day8_test_input.txt"))

(def known? #{2 3 4 7})

(defn calc-task-1
  [input]
  (->> input
       (map second)
       flatten
       (filter (fn [p]
                 (known? (count p))))
       count))

(def number-segments
  {#{:t :tr :br :b :bl :tl} 0
   #{:tr :br} 1
   #{:t :tr :m :bl :b} 2
   #{:t :tr :m :br :b} 3
   #{:tl :m :tr :br} 4
   #{:t :tl :m :br :b} 5
   #{:t :tl :m :br :b :bl} 6
   #{:t :tr :br} 7
   #{:t :tr :m :tl :br :b :bl} 8
   #{:m :tl :t :tr :br :b} 9})

(def valid-letters (set "abcdefg"))

(defn calc-line-wiring
  [line-patterns]
  (let [patterns (sort-by count (map (comp string/join sort) line-patterns))]
    (reduce (fn [{:keys [tr tl br m letters] :as mapping} pattern]
              (let [p (set pattern)]
                (case (count pattern)
                  2 (merge mapping {:tr p :br p :letters p})
                  3 (merge mapping {:t (set/difference p tr)
                                    :letters (into letters p)})
                  4 (let [diff (set/difference p tr)]
                      (merge mapping {:tl diff
                                      :m diff
                                      :letters (into letters p)}))
                  5 (let [diff (set/difference p letters)
                          l (into letters diff)]
                      (if (= 1 (count diff))
                        (merge mapping {:b diff
                                        :bl (set/difference valid-letters l)})
                        (merge mapping {:m (set/intersection m p)
                                        :tl (set/difference tl p)
                                        :tr (set/intersection tr p)})))
                  (reduced
                   (as-> mapping n
                     (dissoc n :letters)
                     (assoc n :br (set/difference br tr))
                     (map (fn [[k l]]
                            [(first l) k])
                          n)
                     (into {} n))))))
            {}
            patterns)))

(defn signal->number
  [line-wiring signal]
  (->> signal
       (map line-wiring signal)
       (set)
       (number-segments)))

(defn calc-line
  [[patterns signals]]
  (let [line-wiring (calc-line-wiring patterns)]
    (->> signals
         (map (partial signal->number line-wiring))
         (string/join)
         (u/parse-int))))

(defn calc-task-2
  [input]
  (->> input
       (map calc-line)
       (apply +)))

(comment
 (calc-task-1 test-input)
 (calc-task-1 day-input)

 (calc-task-2 test-input)
 (calc-task-2 day-input)
 )
