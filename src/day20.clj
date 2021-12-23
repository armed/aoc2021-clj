(ns day20
  (:require [hyperfiddle.rcf :refer [tests]]
            [clojure.string :as string]
            [utils :as u]))

(hyperfiddle.rcf/enable!)

(defn line->vec
  [line]
  (when (seq line)
    (mapv (fn [c] (if (= \. c) 0 1)) line)))

(defn parse-input
  [input]
  (let [[algo-line _ & img-lines] (->> input (slurp) (string/split-lines))]
    {:algo (line->vec algo-line)
     :image (mapv line->vec img-lines)}))

(def day-input
  (parse-input "src/day20_input.txt"))

(def test-input
  (parse-input "src/day20_test_input.txt"))

(defn neigbour-pixels
  [image [r c] void-color]
  (map #(get-in image % void-color)
       [[(dec r) (dec c)]
        [(dec r) c]
        [(dec r) (inc c)]
        [r (dec c)]
        [r c]
        [r (inc c)]
        [(inc r) (dec c)]
        [(inc r) c]
        [(inc r) (inc c)]]))

(defn algo-index
  [image rc void-color]
  (u/parse-int
   (string/join
    (neigbour-pixels image rc void-color))
   2))

(defn enhance-pixel
  [image algo rc void-color]
  (let []
    (->> void-color
         (algo-index image rc)
         (nth algo))))

(defn enhance-image
  [image algo step]
  (let [size (count image)
        void-color (cond
                     (zero? (first algo)) 0
                     (zero? step) 0
                     (even? step) 0
                     :else 1)]
    (->> (for [r (range -1 (inc size))
               c (range -1 (inc size))]
           (enhance-pixel image algo [r c] void-color))
         (partition (+ 2 size))
         (mapv vec))))

(defn print-image
  [image]
  (doseq [line (map (fn [row]
                      (->> row
                           (map #(if (zero? %) \. \#))
                           string/join))
                    image)]
    (println line))
  image)

(defn count-lit-pixels
  [image]
  (reduce (fn [acc row]
            (apply + acc row))
          0
          image))

(defn calc-task
  [{:keys [image algo]} steps]
  (->> (range steps)
       (reduce (fn [img step]
                 (enhance-image img algo step))
               image)
       #_(print-image)
       (count-lit-pixels)))

(comment
 (calc-task day-input 2) ; 5571
 (calc-task day-input 50) ; 17965
 )

(tests
  (calc-task test-input 2) := 35
  (calc-task test-input 50) := 3351

  (algo-index [[1 0 1]
               [0 1 0]
               [0 0 1]]
              [1 1] 0)
  := (u/parse-int "101010001" 2)

  (algo-index [[1 0 1]
               [0 0 1]
               [0 0 1]]
              [1 1] 0)
  := (u/parse-int "101001001" 2)
  )
