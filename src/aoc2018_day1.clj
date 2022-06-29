(ns aoc2018-day1
  (:require [clojure.string]))



(defn part-1
  [items]
  (reduce + items))


;; 주어진 컬렉션에서 반복되는 원소가 있으면 이를 반환하는 함수

(defn get-first-duplicated
  [coll]
  (reduce (fn [seen-set value]
            (if (seen-set value)
              (reduced value)
              (conj seen-set value)))
          #{}
          coll))

;; 주어진 컬렉션을 누적 합산한 lazy-seq를 만들고 이를 get-first-duplicated 의 인자로 넘긴다
(defn part-2
  [coll]
  (let [freqs (->> coll cycle (reductions + 0))]
    (get-first-duplicated freqs)))


(comment
  (def input-data (->> (slurp "../data/aoc2018_day1_data")
                       (clojure.string/split-lines)
                       (map #(Integer/parseInt %))))
  (part-1 input-data)
  (part-2 input-data)
)



(defn part-2-with-loop
  [items]
  (loop [result-set #{}
         acc 0
         [x & coll' :as coll] (cycle items)]
    (if (contains? result-set (+ acc x))
      (+ acc x)
      (recur (conj result-set (+ acc x))
             (+ acc x)
             coll'))))
)

(defn part-2-refactored
  [items]
  (loop [freq-history (->> items cycle (reductions + 0))
        seen-freqs #{}]
    (if (seen-freqs (first freq-history))
      (first freq-history)
      (recur (rest freq-history) (conj seen-freqs (first freq-history))))))

(defn part-2-refactored2
  [items]
  (let [freq-history (->> items cycle (reductions + 0))]
    (reduce (fn [seen-freqs new-val]
              (if (seen-freqs new-val)
                (reduced new-val)
                (conj seen-freqs new-val)))
            #{}
            freq-history)))

(defn find-first-duplicated
  [coll]
  (reduce (fn [seen-set new-val]
              (if (seen-set new-val)
                (reduced new-val)
                (conj seen-set new-val)))
            #{}
            coll))

(defn part-2-refactored3
  [items]
  (let [freq-history (->> items cycle (reductions + 0))]
    (find-first-duplicated freq-history)))



(reduce (fn [a b] (reduced :this-is-my-answer))
        (iterate inc 0))

(reduce + [1 2 3 4 5])


    


(comment
(def input-data (->> (slurp "aoc2018_day1_data")
                     (clojure.string/split-lines)
                     (map #(Integer/parseInt %))))
(part-2 [+3, +3, +4, -2, -4, +4])
(part-2-refactored [+3, +3, +4, -2, -4, +4])
(part-2-refactored input-data)
(part-2-refactored2 input-data)
(part-2-refactored3 input-data)

(conj '(1 2 3 4) 5)
(reductions + [+3, +3, +4, -2, -4, +4])
(part-2 input-data)


)




(comment
  (reductions + [0, +3, +3, +4, -2, -4])
  (reductions + [0, -6, +3, +8, +5, -6])
  (reductions + [0, +1, -1])
  (contains? 1 '(1 2 3))
  (conj #{1 2 3} 4)

(def data (cycle input-data))
(part-2 data)

  
  
)


                       
 
  
