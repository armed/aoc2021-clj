(ns task4
  (:require [clojure.string :as string]
            [utils :as u]))

(def raw-data
  (as-> "src/task4_input.txt" n
    (slurp n)
    (string/split-lines n)))

(defn get-guess-nums
  [raw-data]
  (as-> raw-data n
    (first n)
    (string/split n #",")
    (map u/parse-int n)))

(defn get-game-boards
  [raw-data]
  (loop [lines (rest raw-data)
         boards []]
    (if (seq lines)
      (recur (drop 6 lines)
             (conj boards
                   (transduce
                    (comp
                     (drop 1)
                     (take 5)
                     (map string/trim)
                     (map #(string/replace % #"\s+" " "))
                     (map #(string/split % #" "))
                     (map #(map u/parse-int %)))
                    conj
                    lines)))
      boards)))

(defn rotate-board
  [board]
  (apply map vector board))

(defn seek
  [rows]
  (->> rows
       (map-indexed (fn [idx row]
                      [idx (apply + row)]))
       (filter #(= -5 (last %)))
       ffirst))

(defn seek-row
  [board]
  (when-let [idx (seek board)]
    {:row idx}))

(defn seek-col
  [board]
  (when-let [idx (seek board)]
    {:col idx}))

(defn seek-sequential-marks
  [board]
  (some identity
        [(seek-row board)
         (seek-col (rotate-board board))]))

(defn mark-board
  [nums board]
  (let [ns (set nums)]
    (for [row board]
      (map #(if (contains? ns %) -1 %) row))))

(defn mark-boards
  [nums boards]
  (map (partial mark-board nums) boards))

(defn calc-board-score
  [last-num winner-board]
  (->> winner-board
       flatten
       (filter #(< 0 %))
       (apply +)
       (* last-num)))

(defn calc-result-1
  [raw-data]
  (let [guess-nums (get-guess-nums raw-data)
        game-boards (get-game-boards raw-data)]
    (loop [nums (take 5 guess-nums)
           rest-nums (drop 5 guess-nums)
           marked-boards (mark-boards nums game-boards)]
      (if-let [winner-boards (seq (filter seek-sequential-marks marked-boards))]
        (calc-board-score (last nums)
                          (first winner-boards))
        (recur (take 1 rest-nums)
               (rest rest-nums)
               (mark-boards [(first rest-nums)] marked-boards))))))

(defn calc-result-2
  [raw-data]
  (let [guess-nums (get-guess-nums raw-data)
        game-boards (get-game-boards raw-data)]
    (loop [nums (take 5 guess-nums)
           rest-nums (drop 5 guess-nums)
           marked-boards (mark-boards nums game-boards)]
      (if (= 1 (count marked-boards))
        (calc-board-score (last nums)
                          (first marked-boards))
        (recur (take 1 rest-nums)
               (rest rest-nums)
               (->> marked-boards
                    (filter #(not (seek-sequential-marks %)))
                    (mark-boards [(first rest-nums)])))))))

(comment
 (calc-result-1 raw-data)
 (calc-result-2 raw-data)
 (some identity [nil nil 3 (map prn [1 2 3]) nil])
 (let [raw-data ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
                 ""
                 "22 13 17 11 0"
                 "8 2 23 4 24"
                 "21 9 14 16 7"
                 " 6 10 3 18 5"
                 "1 12 20 15 19"
                 ""
                 " 3 15 0 2 22"
                 "9 18 13 17 5"
                 "19 8 7 25 23"
                 "20 11 10 24 4"
                 "14 21 16 12 6"
                 ""
                 "14 21 17 24 4"
                 "10 16 15 9 19"
                 "18 8 23 26 20"
                 "22 11 13 6 5"
                 "2 0 12 3 7"]]
   (calc-result-2 raw-data))

 ((-1 -1 -1 -1 -1)
  (-1 -1 15 -1 19)
  (18 8 -1 26 20)
  (22 -1 -1 6 -1)
  (-1 -1 12 3 -1))



 (let [board ['(59 93 18 63 19)
              '(92 14 61 13 26)
              '(39 70 2 58 6)
              '(68 57 89 81 4)
              '(55 98 79 85 3)]
       nums [92 89 81 85 98]]
   (mark-board nums board))

 (seek-sequential-marks
  '((59 93 18 63 19)
    (-1 14 61 13 26)
    (39 70 2 58 6)
    (-1 -1 -1 -1 -1)
    (55 -1 79 -1 3)))

 ((59 93 18 63 19)
  (92 14 61 13 26)
  (39 70 2 58 6)
  (68 57 89 81 4)
  (55 98 79 85 3)))