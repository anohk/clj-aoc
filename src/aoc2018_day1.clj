(ns aoc2018-day1
  (:require [clojure.string]))

(def input-data (->> (slurp "data/aoc2018_day1_data")
                     (clojure.string/split-lines)
                     (map #(Integer/parseInt %))))

;; --------------
;; part 1

(defn solve-part1
  [items]
  (reduce + items))


;; --------------
;; part 2

(defn get-first-duplicated
  [coll]
  (reduce (fn [seen-set value]
            (if (seen-set value)
              (reduced value)
              (conj seen-set value)))
          #{}
          coll))

(defn solve-part2
  [coll]
  (let [freqs (->> coll cycle (reductions + 0))]
    (get-first-duplicated freqs)))


(comment
  (solve-part1 input-data)
  (solve-part2 input-data))



;; -------------------------------
;; loop, recur / reduce를 이용한 풀이
(defn solve-part2-with-loop1
  [items]
  (loop [result-set #{}
         acc 0
         [x & coll' :as coll] (cycle items)]
    (if (contains? result-set (+ acc x))
      (+ acc x)
      (recur (conj result-set (+ acc x))
             (+ acc x)
             coll'))))

(defn solve-part2-with-loop2
  [items]
  (loop [freq-history (->> items cycle (reductions + 0))
         seen-freqs #{}]
    (if (seen-freqs (first freq-history))
      (first freq-history)
      (recur (rest freq-history) (conj seen-freqs (first freq-history))))))

(defn solve-part2-with-reduce
  [items]
  (let [freq-history (->> items cycle (reductions + 0))]
    (reduce (fn [seen-freqs new-val]
              (if (seen-freqs new-val)
                (reduced new-val)
                (conj seen-freqs new-val)))
            #{}
            freq-history)))


(comment
  (solve-part2-with-loop1 input-data)
  (solve-part2-with-loop2 input-data)
  (solve-part2-with-reduce input-data))
