(ns task3
  (:require [clojure.string :as string]
            [utils :as u]))

(def lines
  (->> "src/task3_input.txt"
       (slurp)
       (string/split-lines)
       (transduce
        (comp
         (map #(string/split % #""))
         (map #(map u/parse-int %)))
        conj)))

(defn get-common-bits
  [bit lines]
  (let [half (/ (count lines) 2)
        comp-fn (if (zero? bit) < >)]
    (as-> lines n
      (apply map + n)
      (map (fn [v]
             (cond
               (= v half) bit
               (comp-fn v half) 1
               :else 0))
           n))))

(defn bit-line-as-int
  [l]
  (u/parse-int (string/join l) 2))

(defn get-param
  [bit]
  (->> lines
       (get-common-bits bit)
       (bit-line-as-int)))

(defn get-param-2
  [bit]
  (loop [lx lines
         pos 0]
    (let [lc (count lx)]
      (if (= 1 lc)
        (-> lx first bit-line-as-int)
        (let [bits (get-common-bits bit lx)]
          (recur (filter #(= (nth % pos) (nth bits pos)) lx)
                 (inc pos)))))))

(defn calc-result-1
  []
  (* (get-param 1) (get-param 0)))

(defn calc-result-2
  []
  (* (get-param-2 1) (get-param-2 0)))

(comment
 (calc-result-1)
 (calc-result-2))
