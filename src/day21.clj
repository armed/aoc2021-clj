(ns day21
  (:require [hyperfiddle.rcf :refer [tests]]))

(hyperfiddle.rcf/enable!)

; puzzle input
(def player1 8)
(def player2 7)

(defn move [pos moves]
  (let [m (mod (+ pos moves) 10)]
    (if (zero? m) 10 m)))

(def run-turn
  (fn [[a b c] [pos score]]
    (let [moves (+ a b c)
          board-num (move pos moves)]
      [board-num (+ score board-num)])))

(defn run-game
  ([players rolls win-score]
   (run-game players rolls win-score 0))
  ([players [roll & rolls] win-score turn]
   (let [scores (map second (vals players))]
     (cond
       (>= (apply max scores) win-score)
       {:players players
        :turn turn}

       :else
       (recur (update players (if (even? turn) :p1 :p2) #(run-turn roll %))
              rolls win-score (inc turn))))))

(defn calc-task-1
  [pos1 pos2]
  (let [dice (->> (range 1 101) (cycle) (partition 3))
        players {:p1 [pos1 0] :p2 [pos2 0]}
        {:keys [players turn]} (run-game players dice 1000)]
    (* (* turn 3) (min (second (:p1 players))
                       (second (:p2 players))))))

(def dirac
  (memoize
   (fn [players pkey]
     (loop [[roll & rolls] (for [a [1 2 3] b [1 2 3] c [1 2 3]] [a b c])
            wins {:p1 0 :p2 0}]
       (if roll
         (let [[pos score] (pkey players)
               [p s] (run-turn roll [pos score])
               players' (assoc players pkey [p s])]
           (if (> s 20)
             (recur rolls (update wins pkey inc))
             (recur rolls
                    (merge-with
                     + wins (dirac players' (if (= :p1 pkey) :p2 :p1))))))
         wins)))))

(defn calc-task-2
  [p1 p2]
  (apply max (vals (dirac {:p1 [p1 0] :p2 [p2 0]} :p1))))


(comment
 (calc-task-1 player1 player2)
 (calc-task-2 player1 player2)
 )

(tests
 (calc-task-1 4 8) := 739785
 (calc-task-2 4 8) := 444356092776315
 )
