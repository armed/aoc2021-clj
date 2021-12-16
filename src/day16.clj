(ns day16
  (:require [clojure.string :as string]
            [utils :as u]
            [hyperfiddle.rcf :refer [tests]]))

(hyperfiddle.rcf/enable!)

(defn parse-int-2
  [bytes]
  (u/parse-int bytes 2))

(def hexmap
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100"
   \5 "0101" \6 "0110" \7 "0111" \8 "1000" \9 "1001"
   \A "1010" \B "1011" \C "1100" \D "1101" \E "1110"
   \F "1111"})

(defn decode
  [hex]
  (string/join (mapv #(get hexmap %) hex)))

(defn parse-input
  [input-file-name]
  (->> input-file-name (slurp) (decode)))

(def day-input (parse-input "src/day16_input.txt"))

(def MIN_BODY_SIZE 2)
(def HEADER_SIZE 6)
(def LEN_0_SIZE 15)
(def LEN_1_SIZE 11)

(def op? (partial not= 4))

(defn valid-body?
  [body]
  (>= (count body) MIN_BODY_SIZE))

(defn parse-header
  [header]
  (let [version (apply str (take 3 header))
        type (apply str (drop 3 header))]
    [(parse-int-2 version) (parse-int-2 type)]))

(defn read-literal
  [initial-body]
  (loop [nums []
         body initial-body
         total-groups 1]
    (let [bit (first body)
          body (rest body)
          [current rest-body] (split-at 4 body)
          num current]
      (if (= \0 bit)
        (let [data (string/join (into nums num))]
          {:body rest-body :data data})
        (recur (into nums num)
               rest-body
               (inc total-groups))))))

(declare read-packet)

(defn bits->int
  [bits-amount body]
  (-> (take bits-amount body) (string/join) (parse-int-2)))

(defn read-op-0
  [op-body]
  (let [total-bits (bits->int LEN_0_SIZE op-body)]
    (loop [sub-body (->> op-body (drop LEN_0_SIZE) (take total-bits))
           sub-packets []]
      (if (valid-body? sub-body)
        (let [{:keys [body packets]} (read-packet sub-body)]
          (recur body (into sub-packets packets)))
        {:body (string/join (drop (+ LEN_0_SIZE total-bits) op-body))
         :packets sub-packets}))))

(defn read-op-1
  [op-body]
  (loop [total-packets (bits->int 11 op-body)
         sub-body (drop LEN_1_SIZE op-body)
         sub-packets []]
    (if (zero? total-packets)
      {:body sub-body :packets sub-packets}
      (let [{:keys [body packets]} (read-packet sub-body)]
        (recur (dec total-packets) body (into sub-packets packets))))))

(defn read-op
  [input]
  (let [length-type-id (first input)
        op-body (rest input)]
    (if (= \0 length-type-id)
      (read-op-0 op-body)
      (read-op-1 op-body))))

(defn read-packet
  [input]
  (let [data-body (drop HEADER_SIZE input)
        [v t] (parse-header (take HEADER_SIZE input))
        {:keys [body packets data]} (if (op? t)
                                      (read-op data-body)
                                      (read-literal data-body))]
    {:body (string/join body)
     :packets [{:version v :type t :packets packets :data data}]}))

(defn calc-version-sum
  [parse-data]
  (loop [packets (:packets parse-data)
         sum 0]
    (if (seq packets)
      (recur (flatten (filter identity (map :packets packets)))
             (apply + sum (map :version packets)))
      sum)))

(defn calc-operations
  [{:keys [type data packets]}]
  (let [[a b & _ :as values] (map calc-operations packets)]
    (case type
      0 (apply + values)
      1 (apply * values)
      2 (apply min values)
      3 (apply max values)
      4 (parse-int-2 data)
      5 (if (> a b) 1 0)
      6 (if (< a b) 1 0)
      7 (if (= a b) 1 0))))

(defn read-packets
  [input]
  (-> input read-packet :packets first))

(tests
 ; task 1
 (calc-version-sum (read-packet day-input))
 := 974
 ; task 2
 (calc-operations (read-packets day-input))
 := 180616437720
 )

(tests
 (read-packets "00111000000000000110111101000101001010010001001000000000")
 := {:version 1,
     :type 6,
     :packets [{:version 6,
                :type 4,
                :packets nil,
                :data "1010"}
               {:version 2,
                :type 4,
                :packets nil,
                :data "00010100"}],
     :data nil}

 (read-packets "110100101111111000101000")
 := {:version 6,
     :type 4,
     :packets nil,
     :data "011111100101"}

 (read-packets "11101110000000001101010000001100100000100011000001100000")
 := {:version 7,
     :type 3,
     :packets [{:version 2,
                :type 4,
                :packets nil,
                :data "0001"}
               {:version 4,
                :type 4,
                :packets nil,
                :data "0010"}
               {:version 1,
                :type 4,
                :packets nil,
                :data "0011"}],
     :data nil}

 (read-packets (decode "8A004A801A8002F478"))
 := {:version 4,
     :type 2,
     :packets [{:version 1,
                :type 2,
                :packets [{:version 5,
                           :type 2,
                           :packets [{:version 6,
                                      :type 4,
                                      :packets nil,
                                      :data "1111"}],
                           :data nil}],
                :data nil}],
     :data nil}

 (read-packets (decode "620080001611562C8802118E34"))
 := {:version 3,
     :type 0,
     :packets [{:version 0,
                :type 0,
                :packets [{:version 0,
                           :type 4,
                           :packets nil,
                           :data "1010"}
                          {:version 5,
                           :type 4,
                           :packets nil,
                           :data "1011"}],
                :data nil}
               {:version 1,
                :type 0,
                :packets [{:version 0,
                           :type 4,
                           :packets nil,
                           :data "1100"}
                          {:version 3,
                           :type 4,
                           :packets nil,
                           :data "1101"}],
                :data nil}],
     :data nil}

 (read-packets (decode "C0015000016115A2E0802F182340"))
 := {:version 6,
     :type 0,
     :packets [{:version 0,
                :type 0,
                :packets [{:version 0,
                           :type 4,
                           :packets nil,
                           :data "1010"}
                          {:version 6,
                           :type 4,
                           :packets nil,
                           :data "1011"}],
                :data nil}
               {:version 4,
                :type 0,
                :packets [{:version 7,
                           :type 4,
                           :packets nil,
                           :data "1100"}
                          {:version 0,
                           :type 4,
                           :packets nil,
                           :data "1101"}],
                :data nil}],
     :data nil}


 (read-packets (decode "A0016C880162017C3686B18A3D4780"))
 := {:version 5,
     :type 0,
     :packets [{:version 1,
                :type 0,
                :packets [{:version 3,
                           :type 0,
                           :packets [{:version 7,
                                      :type 4,
                                      :packets nil,
                                      :data "0110"}
                                     {:version 6,
                                      :type 4,
                                      :packets nil,
                                      :data "0110"}
                                     {:version 5,
                                      :type 4,
                                      :packets nil,
                                      :data "1100"}
                                     {:version 2,
                                      :type 4,
                                      :packets nil,
                                      :data "1111"}
                                     {:version 2,
                                      :type 4,
                                      :packets nil,
                                      :data "1111"}],
                           :data nil}],
                :data nil}],
     :data nil}

 (calc-version-sum (read-packet (decode "8A004A801A8002F478")))
 := 16

 (calc-version-sum (read-packet (decode "620080001611562C8802118E34")))
 := 12

 (calc-version-sum (read-packet (decode "C0015000016115A2E0802F182340")))
 := 23

 (calc-version-sum (read-packet (decode "A0016C880162017C3686B18A3D4780")))
 := 31

 (calc-operations (read-packets (decode "C200B40A82"))) := 3

 (calc-operations (read-packets (decode "04005AC33890"))) := 54

 (calc-operations (read-packets (decode "880086C3E88112"))) := 7

 (calc-operations (read-packets (decode "CE00C43D881120"))) := 9

 (calc-operations (read-packets (decode "D8005AC2A8F0"))) := 1

 (calc-operations (read-packets (decode "F600BC2D8F"))) := 0

 (calc-operations (read-packets (decode "9C005AC2F8F0"))) := 0

 (calc-operations (read-packets (decode "9C0141080250320F1802104A08"))) := 1

 (read-packets (decode "9C0141080250320F1802104A08"))
 := {:version 4,
     :type 7,
     :packets [{:version 2,
                :type 0,
                :packets [{:version 2, :type 4, :packets nil, :data "0001"}
                          {:version 4, :type 4, :packets nil, :data "0011"}],
                :data nil}
               {:version 6,
                :type 1,
                :packets [{:version 0, :type 4, :packets nil, :data "0010"}
                          {:version 2, :type 4, :packets nil, :data "0010"}],
                :data nil}],
     :data nil}
 )
