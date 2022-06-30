(ns aoc2018-day3)


;; part1
;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; 위 정보를 기반으로 각 천조각에 해당하는 좌표 시퀀스를 구한다.

;; 천조각 fabric
;; {:id 1
;;  :positions [[1 3] [2 3] ...]}

;; 모든 천조각을 좌표료 비교한다.
;; 한번 이상 등장한 좌표를 찾는다.
;; -> 모든 천조각의 :positions 를 flat하게 만든다 -> 1개의 시퀀스 -> frequencies를 이용해 좌표의 빈도를 구한다. -> 빈도가 1을 초과하는 좌표의 수를 구한다.

;; --------------------------------------
;; 데이터 파싱
;; 1. ID, left, top, wide, tall 정보 추출
;; 2. 천조각 정보 맵으로 데이터 변환

(defn get-positions
  [[left top width height]]
  (for [p1 (range left (+ left width))
        p2 (range top (+ top height))]
    [p1 p2]))

(defn make-fabric-data
  [[id & position-infos]]
  {:id id
   :positions (get-positions position-infos)})


(defn parse-fabric [string]
  (when-let [pattern-matched (first (re-seq #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" string))]
    (->> (next pattern-matched)
         (map #(Integer/parseInt %))
         make-fabric-data)))

(defn parse-fabrics
  [strings]
  (map parse-fabric strings))


(def input-data (->> (slurp "data/aoc2018_day3_data")
                     (clojure.string/split-lines)))

(def fabrics (parse-fabrics input-data))


;; ----------------------------
;; part 1
;; ----------------------------

(defn solve-part-1
  [parsed-data]
  (->> parsed-data
       (map :positions)
       (apply concat)
       frequencies
       (filter #(< 1 (val %)))
       count))

(defn solve-part-1-with-mapcat
  [parsed-data]
  (->> parsed-data
       (mapcat :positions)
       frequencies
       (filter #(< 1 (val %)))
       count))

(solve-part-1 fabrics)

;; -----------------------------------
;; 연오님 코멘트
;; naming
;; single process & map
;; comp vs ->>


(defn parsed-fabric-data
  [input-data]
  (->> input-data
       (map (comp make-fabric-data
                  convert-to-int
                  regex-for-fabric))))

(defn parse-fabric-3
  [input-data]
  (->> input-data
       (map #(->> %
                  regex-for-fabric
                  convert-to-int
                  make-fabric-data))))

(defn parse-fabric [string]
  (->> string
       regex-for-fabric
       convert-to-int
       make-fabric-data))

(parse-fabric "#1 @ 1,3: 4x4")

(defn parse-fabrics [strings]
  (map parse-fabric string))

(defn parse-fabrics [strings]
  (->> strings
       (map regex-for-fabric)
       (map convert-to-int)
       (map make-fabric-data)))

(defn convert-to-int
  [num-str-coll]
  (->> num-str-coll
       (map (fn [num-str] (Integer/parseInt num-str)))))


(defn regex-for-fabric
  [str]
  (->> str
       (re-seq #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
       first
       next))


;; ---------------------------------------


mapcat
(mapcat reverse [[1 2 3] [4 5 6] [7 8 9]])
(map reverse [[1 2 3] [4 5 6] [7 8 9]])
(concat [3 2 1] [6 5 4] [9 8 7])
(apply concat [[3 2 1] [6 5 4] [9 8 7]])

(comment
  (parsed-fabric-data input-data)
  (def fabric-data (parsed-fabric-data input-data))
  
  (= (solve-part-1 fabric-data) 103806)
  (= (solve-part-1-with-mapcat fabric-data) 103806)
  )

;;-------------------------------------
;; part2
;; 겹치는 부분이 없는 천조각 ID 구하기
;; overlap-with-others? 가 false인 것  => 타겟 천조각의 각 좌표를 다른 천조각의 좌표들과 비교하면서 중복 등장하지 않는 것을 찾는다.
;; 모든 천조각에 대해 overlap-with-others? 를 적용. -> filter or remove 이용해서 구하기
;; 문제에서는 답이 하나라는 것을 보장하고 있음


(defn overlap?
  [[fabric1 fabric2]]
  (->> (clojure.set/intersection (set (:positions fabric1))
                                 (set (:positions fabric2)))
       count
       (#(< 0 %))))


(defn find-unique-fabric
  [fabrics fabric]
  (->> fabrics
       (map #(overlap? [fabric %]))
       (filter identity)
       count
       (#(when (= 1 %) fabric))))


(defn solve-part2
  [fabrics]
  (->> fabrics
       (keep (partial find-unique-fabric fabrics))
       first
       :id))

(find-unique-fabric fabrics (take 1100 fabrics))
(find-unique-fabric test-data (last test-data))

(solve-part2 test-data)
(solve-part2 fabrics)


(def test-data
  [{:id 1 :positions [[2 3] [1 2]]}
   {:id 2 :positions [[1 2] [1 3] [4 5]]}
   {:id 3 :positions [[1 1] [2 2] [3 3]]}
   {:id 4 :positions [[2 3] [1 2]]}])


;; -------------------------






(defn solve-part2
  [fabrics]
  (->> fabrics
       (map (fn [fabric1] (map #(->> [fabric1 %]) fabrics)))
       (map find-unique-fabric)
       ))

(solve-part2 test-data)
(solve-part2 [{:id 1 :p "A"} {:id 2 :p "B"}])

(defn find-unique-fabric
  [fabrics]
  (->> fabrics
       (apply )
       (map (fn [fabric1] (map [])))))

(defn test-map
  [fabrics]
  (->> fabrics
       (map #(->> (for [f1 {% %} f2 fabrics] [f1 f2])))))

(test-map [{:id 1 :p 2} {:id 2 :p 3}])
(map #(->> [%]) test-data)

(find-unique-fabric test-data)



(defn find-unique-fabric
  [fabrics fabric]
  (->> fabrics
       (map (partial overlap? ))))



(find-unique-fabric test-data (first test-data))

(count (filter true? [false false]))

(defn solve-part2
  [fabrics]
  (->> fabrics
       (map (partial find-unique-fabric fabrics))))

(solve-part2 test-data)
(find-unique-fabric test-data (first test-data))
(map (partial overlap? (:positions test-data)))






(defn find-unique-fabric
  [fabrics fabric]
  (let [target (:positions fabric)
        compares (concat (mapcat :positions fabrics))]
    (println (map (partial overlap? compares) target))
    (when (every? false? (map (partial overlap? compares) target))
      fabric)))
(map :positions test-data)


(defn solve-part-2-refactored2
  [fabrics]
  (->> fabrics
       (keep (partial find-unique-fabric fabrics))
       ))

(solve-part-2-refactored2 test-data)
(find-unique-fabric test-data (first test-data))
(println test-data)

(defn solve-part-2-refactored
  [fabrics]
  (->> fabrics
       (iterate next)
       (take-while #(< 1 (count %)))
       (keep find-unique-fabric)
       first))




(defn solve-part-2
  [fabrics]
  (let [unique (find-unique-fabric fabrics)]
    (when fabrics
      (if (nil? unique)
        (recur (next fabrics))
        unique))))

