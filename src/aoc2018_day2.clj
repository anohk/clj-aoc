(ns aoc2018_day2
  (:require [clojure.string]))

(def input-data (->> (slurp "data/aoc2018_day2_data")
                     (clojure.string/split-lines)))

;; -------------------------------
;; part1
;; 주어진 문자열에서 문자가 n번 반복하는 것이 있는지 확인한다.


(defn is-duplicated-n-times?
  [n coll]
  (->> coll
       frequencies
       vals
       (some #{n})))

(def twice? (partial is-duplicated-n-times? 2))

(def third? (partial is-duplicated-n-times? 3))

(defn solve-part1
  [input-data]
  (* (count (filter twice? input-data))
     (count (filter third? input-data))))


(comment
  (solve-part1 input-data))


;; -------------------------------
;; part2
;; 주어진 문자열들 중 중복되는 문자가 한번만 있는 경우 중복문자를 제거한 값을 구한다.

(defn n-sized?
  [n coll]
  (= n (count coll)))

(defn filter-n-size
  [n coll]
  (filter (partial n-sized? n) coll))

(defn same-string-except-only-1
  "서로 다른 문자가 1개인 문자열을 반환한.
   - 두 문자열을 순서대로 쌍을 묶고 distinct
   - distinct한 것의 길이가 2개인 것은 동일 위치에 서로 다른 문자가 있는 경우이다.
   - 위와 같은 경우가 1번만 있는 경우를 찾아 동일한 문자만 반환한다."
  [[str1 str2]]
  (let [distincted-strings (map (comp distinct vector) str1 str2)]
    (when (= 1 (count (filter-n-size 2 distincted-strings)))
      (->> (filter-n-size 1 distincted-strings)
           (map first)
           (apply str)))))

(defn solve-part2
  [input-data]
  (->> (for [str1 input-data
             str2 input-data]
         [str1 str2])
       (keep same-string-except-only-1)
       first))

(comment
  (solve-part2 input-data))

