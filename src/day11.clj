(ns day11
  (:require [clojure.string :as string]
            [utils :as u]
            [clojure.set :as set]))

(defn parse-input
  [file-name]
  (->> file-name
       (slurp)
       (string/split-lines)
       (mapv #(mapv (comp u/parse-int str) %))))

(def test-input (parse-input "src/day11_test_input.txt"))

(def day-input (parse-input "src/day11_input.txt"))

(defn adjacents
  [row col size]
  (for [r (range (dec row) (+ 2 row))
        c (range (dec col) (+ 2 col))
        :when (and (>= r 0) (< r size)
                   (>= c 0) (< c size)
                   (not= [row col] [r c]))]
    [r c]))

(defn perform-step
  [cave-state]
  (mapv #(mapv inc %) cave-state))

(defn find-max-energy
  [cave-state]
  (let [size (count cave-state)]
    (seq
     (for [r (range 0 size)
           c (range 0 size)
           :when (> (get-in cave-state [r c]) 9)
           :let [adj (filter (fn [rc]
                               (<= (get-in cave-state rc) 9))
                             (adjacents r c size))]]
       {:max [r c]
        :adj adj}))))

(defn raise-energy
  [cave-state flashed octopuses]
  (reduce (fn [state [r c]]
            (if (contains? flashed [r c])
              state
              (update-in state [r c] #(if (<= % 9) (inc %) %))))
          cave-state
          octopuses))

(defn print-cave
  [cave-state]
  (doseq [row cave-state]
    (println (string/join (map #(if (zero? %) "-" %) row))))
  (println)
  cave-state)

(defn perform-flash
  [{:keys [state flashed]} energy-data]
  (reduce (fn [{:keys [state flashed]} {[r c] :max adj :adj}]
            (let [flashed (conj flashed [r c])
                  new-state (assoc-in state [r c] 0)]
              {:state (raise-energy new-state flashed adj)
               :flashed flashed}))
          {:state state
           :flashed (or flashed #{})}
          energy-data))

(defn run-step
  [cave-state]
  (loop [state cave-state
         energy (find-max-energy state)
         flashed #{}]
    (if energy
      (let [{:keys [state flashed]} (perform-flash {:state state
                                                    :flashed flashed} energy)]
        (recur state
               (find-max-energy state)
               flashed))
      [state (count flashed)])))

(defn calc-task-1
  [cave-state]
  (loop [state cave-state
         steps-left 100
         cnt 0]
    (if (zero? steps-left)
      cnt
      (let [[state flashed-count] (run-step (perform-step state))]
        (recur state
               (dec steps-left)
               (+ cnt flashed-count))))))

(defn all-flash?
  [cave-state]
  (zero? (apply + (flatten cave-state))))

(defn calc-task-2
  [cave-state]
  (loop [state cave-state
         step 0]
    (if (all-flash? state)
      step
      (let [[state] (run-step (perform-step state))]
        (recur state (inc step))))))

(comment
 (calc-task-1 test-input)
 (calc-task-2 test-input)

 (calc-task-1 day-input)
 (calc-task-2 day-input)
 )