(ns aoc2018_day6)

(def input-data (->> (slurp "../data/aoc2018_day6_data")
                     (clojure.string/split-lines)))

;; ----------------------------------------------------------
;; part1

;; ------------
;; * 사용할 데이터
;; ------------
;;
;; - target들의 좌표와 근거리 좌표
;; [{:target [13 52]
;;   :area [[12 52] [13 52] [14 52] ...]}
;;  {:target [5 10]
;;   :area [[4 10] [6 10] [5 11] ...]}
;;  ...]
;;
;; -------
;; 풀이 계획
;; -------
;;
;; * 두 좌표간 맨하탄 거리를 계산하는 함수
;; * 근거리 좌표 지정 함수
;;   - 특정 좌표에 대해 어떤 target의 영역인지 계산한다.
;;   - 다른 target들의 :area에 속하면 계산하지 않는다.
;;   - 지정되지 않은 좌표의 경우, 모든 target과 맨하탄 거리를 계산하여 가장 짧은 target의 :area에 추가한다.
;;   - 가장 짧은 맨하탄 거리를 갖는 target이 2개 이상인 경우, :area에 추가하지 않는다.
;; * target들의 area 중 길이가 가장 긴 것의 길이를 구한다.
;;
;; ----------------------------
;; * 근거리 좌표를 계산할 영역을 구하기
;; ----------------------------
;;
;;   1. 주어진 좌표 중 상하좌우 가장 끝에 있는 4개의 target을 구한다.
;;
;;      . . . . . . . . . . . .
;;      . . . . . . . B . . . .
;;      . . . . . . . . . . . .
;;      . A . . . . . . . . . .
;;      . . . . . . . . . . . .
;;      . . . . . . . . . . . .
;;      . . . . . . . . . . D .
;;      . . . C . . . . . . . .
;;      . . . . . . . . . . . .
;;
;;   2. 4개의 target 좌표를 기준으로 경계 영역을 구한다.
;;      - 영역 내부에 있는 target은 제외한다 -> (. 으로 표시된 곳이 계산할 좌표)
;;
;;      . . . . . . B . .
;;      . . . . . . . . .
;;      A . . E . . . . .
;;      . . . . . . H . .
;;      . . G . F . . . .
;;      . . . . . . . . D
;;      . . C . . . . . .
;;
;;
;; ---------------------
;; * 무한 증식 target 구하기
;; ---------------------
;;
;;   - 가장 끝 자리에 있는 4개의 좌표는 무한 증식 가능한 target이다.
;;   - 4개의 target을 기준으로 하는 경계 좌표가 :area에 포함되면 무한 증식 가능한 target이다.
;; 
;;   - 4개의 target 좌표를 기준으로 경계 좌표를 구한다. (# 표시된 부분)
;;      - 경계좌표 내에 있는 좌표에 대해 근거리 좌표 계산을 한다. (# 포함)
;;
;;      # # # # # # B # #
;;      # . . . . . . . #
;;      A . . . . . . . #
;;      # . . . . . . . #
;;      # . . . . . . . #
;;      # . . . . . . . D
;;      # # C # # # # # #
;;


(defn get-border-range
  [coordinates]
  (let [sort-by-x (sort-by #(first %) coordinates)
        sort-by-y (sort-by #(last %) coordinates)]
    {:x-positions (map first [(first sort-by-x) (last sort-by-x)])
     :y-positions (map last [(first sort-by-y) (last sort-by-y)])}))


;; 연오님이 나중에 이야기할 것 있다고 함
(defn inclusive-range
  [start end]
  (-> (vec (range start end))
      (conj end)))

(defn make-coordinate
  [x y-positions]
  (->> y-positions
      (map #(vector x %))))

(defn remove-targets
  [target-coordinates all-coordinates]
  (clojure.set/difference (set all-coordinates) (set target-coordinates)))


(defn get-border-coordinates
  ;; 경계 라인의 좌표를 구한다.
  [coordinates]
  (let [border-range (get-border-range coordinates)
        x-positions (apply inclusive-range (:x-positions border-range))
        y-positions (apply inclusive-range (:y-positions border-range))
        min-x (first x-positions)
        max-x (last x-positions)
        min-y (first y-positions)
        max-y (last y-positions)]

    (vec (distinct (concat (map #(vector min-x %) y-positions)
                           (map #(vector max-x %) y-positions)
                           (map #(vector % min-y) x-positions)
                           (map #(vector % max-y) x-positions))))))


(defn get-unspecified-coordinates
  ;; target 좌표를 제외하고 근거리 좌표 계산을 할 좌표 시퀀스
  [coordinates]
  (let [border-range (get-border-range coordinates)
        x-positions (apply inclusive-range (:x-positions border-range))
        y-positions (apply inclusive-range (:y-positions border-range))]
    (->> (for [x x-positions y y-positions] [x y])
         (#(remove-targets coordinates %)))))


(defn calculate-manhattan-distance
  [coordinate1 coordinate2]
  (+ (abs (- (first coordinate1) (first coordinate2)))
     (abs (- (last coordinate1) (last coordinate2)))))


(defn find-closest-target
  [coordinate targets]
  (let [sorted-targets (sort-by #(calculate-manhattan-distance coordinate (:target %)) targets)]
    (when (not= (calculate-manhattan-distance coordinate (:target (first sorted-targets)))
                (calculate-manhattan-distance coordinate (:target (second sorted-targets))))
      (first sorted-targets))))


(defn specify-coordinate
  [targets coordinate]
  (let [closest-target (find-closest-target coordinate targets)]
    (if closest-target
      (update-in targets
                 [(.indexOf targets closest-target) :area]
                 #(conj % coordinate))
      targets)))

(defn include-border?
  [border-coordinate target]
  (seq (clojure.set/intersection (set (:area target))
                                 (set border-coordinate))))


(defn convert-to-coordinate
  [coordinate-string]
  (->>(re-seq #"(\d+), (\d+)" coordinate-string)
      first
      next
      (map #(Integer/parseInt %))))


(defn solve-part1
  [input-data]
  (let [coordinates (map convert-to-coordinate input-data)
        targets (mapv #(hash-map :target (vec %) :area []) coordinates)
        unspecified-coordinates (get-unspecified-coordinates coordinates)
        border-coordinate (get-border-coordinates coordinates)]
    (->> unspecified-coordinates
         (reduce specify-coordinate targets)
         (remove #(include-border? border-coordinate %))
         (map #(count (:area %)))
         sort
         last
         inc)))

(solve-part1 input-data)

;; inc --> 제거했던 target 좌표 추가해야 함


