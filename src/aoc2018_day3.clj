(ns aoc2018-day3)

;; -------------------------
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


(defn make-fabric-map
  [[id & position-infos]]
  {:id id
   :positions (get-positions position-infos)})


(defn parse-to-fabric [string]
  (when-let [pattern-matched (first (re-seq #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" string))]
    (->> (next pattern-matched)
         (map #(Integer/parseInt %))
         make-fabric-map)))

(defn get-fabrics
  [strings]
  (map parse-to-fabric strings))


(def input-data (->> (slurp "data/aoc2018_day3_data")
                     (clojure.string/split-lines)))

(def fabrics (get-fabrics input-data))


;; ----------------------------
;; part 1


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

;;-------------------------------------
;; part2
;; 겹치는 부분이 없는 천조각 ID 구하기
;; 타겟 천조각의 각 좌표를 다른 천조각의 좌표들과 비교하면서 중복 등장하지 않는 것을 찾는다.
;; 문제에서는 답이 하나라는 것을 보장하고 있음


(defn overlap?
  [[fabric1 fabric2]]
  (let [positions1 (:positions fabric1)
        positions2 (:positions fabric2)]
    (->> (clojure.set/intersection (set positions1)
                                   (set positions2))
         count
         (#(< 0 %)))))


(defn find-unique-fabric
  "다른 천조각들과 겹치지 않는 고유한 천조각을 찾는다
   - fabrics 전체와 개별 fabric을 비교하여 중복이 1개인 것을 찾는다. (자기 자신과의 중복)"
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

(comment
  (solve-part2 fabrics))

