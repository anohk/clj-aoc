(ns aoc2018_day2
  (:require [clojure.string]))


;; 주어진 문자열에서 문자가 n번 반복하는 것이 있는지 확인한다

(defn is-duplicated-n-times?
  [n coll]
  (->> coll
       frequencies
       vals
       (some #{n})))

(def twice? (partial is-duplicated-n-times? 2))

(def third? (partial is-duplicated-n-times? 3))


(defn part-1
  [input-data]
  (* (count (filter twice? input-data))
     (count (filter third? input-data))))


(comment
  (def input-data (->> (slurp "data/aoc2018_day2_data")
                       (clojure.string/split-lines)))
  (part-1 input-data)

  )




;; -------------------------------
;; part2

(defn n-sized?
  [n coll]
  (= n (count coll)))

(defn filter-n-sized-strings
  [n strings]
  (filter (partial n-sized? n)
          strings))

(defn same-except-only-1
  [[str1 str2]]
  (let [distincted-strings (map (comp distinct vector) str1 str2)]
    (when (= 1 (count (filter-n-sized-strings 2 distincted-strings)))
      (->> (filter-n-sized-strings 1 distincted-strings)
           (map first)
           (apply str)))))

(defn solve-part-2
  [input-data]
  (->> (for [str1 input-data
             str2 input-data]
         [str1 str2])
       (keep same-except-only-1)
       first))


(comment
  (def input-data (->> (slurp "aoc2018_day2_data")
                       (clojure.string/split-lines)))
  (= "prtkqyluiusocwvaezjmhmfgx" (solve-part-2 input-data))
  (same-str-of-two-string ["asdf" "asdd"])
  (find-same-str-all ["asdf" "aaaa" "asdd"])
  )



;; -----------------------------------




(for [s1 ["a" "b" "c" "d"]
      s2 [1 2 3 4]
      :when (even? s2)
      :let [s3 (* 2 s2)]]
  [s1 s3])




;------

(defn find-same-str
  [coll target]
  (let [same-str (same-str-of-two-string target (first coll))]
    (when (not (nil? coll))
      (if same-str
        same-str
        (recur (next coll) target)))))

(defn find-same-str-all
  [input-data]
  (first (keep (partial find-same-str input-data) input-data))
  )



;; ------------------------

;; 연오님 조언


;; S S'

(def S [1 2 3])
(map (fn [s]
       (map #(vector s %)
            S))
     S)

(for [s1 S s2 S]
  [s1 s2])
 





;; -----------------------

;; 하나의 문자만 다른 문자열을 찾는다


(defn find-single-diff-str
  [str1 str2]
  (->> (interleave str1 str2)
       (partition 2)
       (map distinct)
       (map count)
       (filter #(not= 1 %))
       count
       )
  )

(defn different-only-one-character?
  [str1 str2]
  (= 1
     (->> (map = str1 str2)
          (filter not)
          count)))




;; 주어진 문자열을 순회하며 각 문자가 키, 등장 횟수가 값인 맵을 생성한다.

(defn my-frequencies
[coll]
(reduce (fn [counter-map item] 
          (if (counter-map item)
            (update counter-map item inc)
            (assoc counter-map item 1)))
        {}
        coll))



;; 맵에서 값이 num인 아이템의 숫자를 센다.

(defn count-items-by-num
[map-coll num]
(reduce (fn [acc map-data]
          (if ((group-by val map-data) num)
            (inc acc)
            acc))
        0
        map-coll))

(defn count-items
[map-coll num]
(->> map-coll
     (filter (fn [map-data]
               ((group-by val map-data) num)))
     count))


(defn get-by-val
[num map-data]
((group-by val map-data) num))

(defn count-items
[map-coll num]
(->> map-coll
     (filter (partial get-by-val num))
     count))


(defn part-1
[input-data]
(let [count-infos (map frequencies input-data)]
  (* (count-items-by-num count-infos 2)
     (count-items-by-num count-infos 3))))

(comment
(reduce (fn [acc str-data]
          (conj acc (frequencies str-data)))
        []
        ["aabbccdef" "aabbdd"])

(map frequencies ["aabbccdef" "aabbdd"]))




(comment
@(def input-data (->> (slurp "aoc2018_day2_data")
                      (clojure.string/split-lines)))
(part-1 input-data)



(reductions (fn [init str-data]
              (conj init (frequencies str-data)))
            []
            input-dcata)

(rest (reductions (fn [init str-data]
                    (conj init (frequencies str-data)))
                  []
                  input-data)
      )
(reductions frequencies ["asdfasdf" "aaaabbb"])
(reductions inc [1 2 3])


(reduce (fn [init str-data]
          (conj init (frequencies str-data)))
        []
        input-data)


(count-items-by-num [{\a 1 \b 2 \c 2 \d 3} {\a 3 \b 1 \c 1 \d 2}] 2)
(count-items-by-num [{\a 1 \b 2 \c 2 \d 3} {\a 2 \b 1 \c 1 \d 2}] 3)
(count-items [{\a 1 \b 2 \c 2 \d 3} {\a 2 \b 1 \c 1 \d 2}] 3)

)


(defn count-items-by-num1
[map-data num]
(count ((group-by val map-data) num)))
