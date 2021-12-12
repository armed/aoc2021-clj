(ns day12
  (:require [clojure.string :as string]))

(def START "start")
(def END "end")

(defn parse-input
  [file-name]
  (->> file-name
       (slurp)
       (string/split-lines)
       (map #(string/split % #"-"))
       (reduce (fn [acc [a b]]
                 (as-> acc m
                   (if (and (not= START b) (not= END a))
                     (update m a conj b)
                     m)
                   (if (and (not= START a) (not= END b))
                     (update m b conj a)
                     m)))
               {})))

(def re-small #"[a-z]+")
(def small-cave? (partial re-matches re-small))
(def test-input (parse-input "src/day12_test_input.txt"))
(def day-input (parse-input "src/day12_input.txt"))

(defn get-joins
  ([cave connections visited-small-caves]
   (get-joins cave connections visited-small-caves false))
  ([cave connections visited-small-caves ignore-dublicates?]
   (->> cave
        (get connections)
        (filterv (fn [n]
                   (if ignore-dublicates?
                     n
                     (not (contains? visited-small-caves n))))))))

(defn path-has-dublicates?
  [path]
  (->> path
       (filter small-cave?)
       (frequencies)
       (vals)
       (apply max)
       (< 1)))

(defn find-paths
  ([{:keys [cave connections current-path single-visits allow-dupes?]
     :or {cave START
          current-path [START]
          single-visits #{START}
          allow-dupes? false}}]
   (if (= END cave)
     [1]
     (let [ignore-dupes? (when allow-dupes?
                           (not (path-has-dublicates? current-path)))
           caves (get-joins
                  cave connections single-visits ignore-dupes?)]
       (mapcat (fn [next-cave]
                 (find-paths
                  {:cave next-cave
                   :connections connections
                   :current-path (conj current-path next-cave)
                   :single-visits (if (small-cave? next-cave)
                                    (conj single-visits next-cave)
                                    single-visits)
                   :allow-dupes? allow-dupes?}))
               caves)))))

(defn calc-task-1
  [connections]
  (->> (find-paths {:connections connections})
       (apply +)))

(defn calc-task-2
  [connections]
  (->> (find-paths {:connections connections
                    :allow-dupes? true})
       (apply +)))

(comment
 (calc-task-1 test-input)
 (calc-task-2 test-input)

 (calc-task-1 day-input)
 (calc-task-2 day-input)

 )