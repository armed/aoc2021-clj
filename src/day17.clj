(ns day17
  (:require [clojure.string :as string]
            [utils :as u]))

; target area: x=253..280, y=-73..-46
(def tx1 253)
(def tx2 280)

(def ty1 -46)
(def ty2 -73)

(def valid-x-range (range 0 (inc tx2)))
(def valid-y-range (range (dec ty2) (- (dec ty2))))

(defn hit?
  [[x y]]
  (and (>= x tx1) (<= x tx2)
       (<= y ty1) (>= y ty2)))

(defn miss?
  [[x y]]
  (or (> x tx2) (< y ty2)))

(def START [0 0])

(defn shoot
  [vx vy]
  (loop [[x y :as pos] START
         vx vx
         vy vy
         max-y Integer/MIN_VALUE]
    (cond
      (hit? pos) [:hit pos max-y]
      (miss? pos) [:miss pos max-y]
      :else
      (let [new-y (+ y vy)]
        (recur [(+ x vx) new-y]
               (cond
                 (> vx 0) (dec vx)
                 (< vx 0) (inc vx)
                 :else vx)
               (dec vy)
               (max max-y new-y))))))

(defn calc-task-1
  []
  (let [myx (for [vx valid-x-range
                  vy valid-y-range
                  :let [[status _ my] (shoot vx vy)]
                  :when (= :hit status)]
              my)]
    (last (sort myx))))

(defn calc-task-2
  []
  (count
   (for [vx valid-x-range
         vy valid-y-range
         :let [[status] (shoot vx vy)]
         :when (= :hit status)]
     [vx vy])))

(comment
 (calc-task-1)
 (calc-task-2)

 )