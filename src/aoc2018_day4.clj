(ns aoc2018_day4)


;; ------------------
;; 최종적으로 가공된 데이터
;; ------------------
;; guard-id
;; slept-min: 날짜별로 잠들어 있던 분의 벡터의 벡터
;; top-frequency: 가장 빈번하게 잠들어있던 분과 빈도를 담은 벡터
;;
;; [{:guard-id 1
;;  :slept-min [[10 11 12 13 14] [14 15 16] [35 36 37 38 39 40 41]]
;;  :top-frequency [14 2]}
;; {:guard-id 2
;;  :slept-min [[5 6 7] [4 5 6] [6 7] [25 26 27 28]]
;;  :top-frequency [6 3]}
;; ...]
;;
;; =============================================================
;;
;; part1 가장 오래 잠들어있던 가드의 ID와 가장 빈번하게 잠들어 있던 분의 곱
;;
;; 가장 오래 잠들어 있던 가드 구하기
;; 1. guard-id 별로 slept-min의 요소의 길이를 모두 더한 값으로 비교하여 가장 큰 값을 가진 guard를 구한다.
;; 2. 해당 guard의 top-frequency의 키 값(가장 빈번하게 잠든 분) 과 guard id의 곱을 구한다.
;;
;;
;; =============================================================
;;
;; part2 같은 분에 가장 많이 잠들어있던 가드의 ID와 해당 분의 곱
;;
;; 1. guard-id 별 top-frequency 데이터 중 빈도가 가장 큰 guard를 구한다.
;; 2. 해당 guard의 top-frequency의 키 값과 guard id의 곱을 구한다. 



;; -----------------------------------------------------------------
;; Parse sleep logs
;; - 로그 파싱 및 1차 가공
;; - {:date "1518-11-01"
;;    :hour 23
;;    :min 59
;;    :msg "fall asleep"}
;; 로그를 파싱하여 위와 같은 모양의 데이터로 변환후, :date :hour :min 순서로 정렬
;; -----------------------------------------------------------------


(def input-data (->> (slurp "data/aoc2018_day4_data")
                     (clojure.string/split-lines)))

(defn convert-to-map
  [[date hour min msg]]
  {:date date
   :hour (Integer/parseInt hour)
   :min (Integer/parseInt min)
   :msg msg})

(defn parse-sleep-log
  [string]
  (->> (re-seq #"\[(\d+-\d+-\d+) (\d+)\:(\d+)] (.*)" string)
       (map next)
       first
       convert-to-map))

(defn parse-sleep-logs
  [strings]
  (->> strings
       (map parse-sleep-log)
       (sort-by (juxt :date :hour :min))))


(def sleep-logs (parse-sleep-logs input-data))


;; ---------------------------------------------------------------------------
;; Group By Guard
;; - 1차 가공한 로그 중 :msg 에 Guard 문자를 포함하는지 확인하여 이를 기준으로 로그를 분리한다.
;; ---------------------------------------------------------------------------
;; 1. split-with
;; split-with 를 사용하면 pred에 따라 1번만 split.
;; 두번째 seq에 대해 split-with를 반복적으로 적용해야한다.
;;
;; 예상되는 문제점: 잠을 자지 않은 가드가 있는 경우 (guard shift log가 연속됨) split-with로 분리가 안됨
;; [G F W G G F W]
;; [G F W] [G G F W] -> [G G F W] 를 split 할 수 없음 -> 결과값이 () (G G F W) 로 반복

;; 2. partition-by
;; pred 의 결과가 바뀌는 기점으로 데이터를 나눈다.
;; [G F W G G F W]
;; [G] [F W] [G G] [F W]
;;
;; 추가작업 필요
;; 모든 guard shift 로그와 fall asleep, wakes up 로그가 분리됨
;; guard shift 와 이어지는 fall asleep, wakes up 로그를 묶어줘야한다. (2개씩 partition 하는 것 적용하면 될듯)
;; guard shift 정보가 두 개 이상이라면 ([G G]와 같은 경우) 마지막 guard shift 정보를 사용하도록한다.



(defn include-guard?
  [log-map]
  (clojure.string/includes? (:msg log-map) "Guard"))


(defn get-guard-id
  [shift-log]
  (->> shift-log
       :msg
       (re-seq #"Guard #(\d+)")
       first
       last
       (Integer/parseInt)))


(defn get-slept-mins
  [sleep-logs]
  (->> sleep-logs
       (partition-all 2)
       (map (fn [[fall-asleep wakes-up]]
              (range (:min fall-asleep) (:min wakes-up))))))


(defn get-top-frequency
  [slept-mins]
  (->> slept-mins
       flatten
       frequencies
       (sort-by val)
       last))


(defn make-guard-record
  [[shift-logs sleep-logs]]
  {:id (get-guard-id (last shift-logs))
   :slept-mins (get-slept-mins sleep-logs)})


(defn merge-by-guard
  [guard-records]
  (->> guard-records
       (group-by :id)
       vals
       (map #(->> {:id (:id (first %))
                   :slept-mins (mapcat :slept-mins %)}))))


(defn add-top-frequency
  [guard-records]
  (->> guard-records
       (#(assoc % :top-frequency (get-top-frequency (:slept-mins %))))))


(defn group-by-guard
  [sleep-logs]
  (->> sleep-logs
       (partition-by (complement include-guard?))
       (clojure.core/partition-all 2)
       (map make-guard-record)
       merge-by-guard
       (map add-top-frequency)))

(def guard-sleep-records (group-by-guard sleep-logs))



;; ----------------------------------------------------------
;; part 1
;; 가장 많이 잔 guard의 id 해당 가드가 가장 빈번하게 잠들었던 분의 곱

(defn total-slept-time
  [guard-sleep-record]
  (->> guard-sleep-record
       :slept-mins
       (map count)
       (reduce +)))


(defn heavy-sleeper
  [guard-sleep-records]
  (->> guard-sleep-records
       (sort-by #(->>(:slept-mins %)
                     (map count)
                     (reduce +)))
       last))


(defn solve-part1
  [guard-sleep-records] ;; input-data를 인자로 받아 처리하도록 변경 필요
  (->> guard-sleep-records
       heavy-sleeper
       (#(* (:id %) (first (:top-frequency %))))))


(comment
  (solve-part1 guard-sleep-records))

;; ----------------------------------------------------------
;; part 2
;; 같은 분에 가장 많이 잠들어있던 가드의 ID와 해당 분의 곱
;;
;; 1. guard-id 별 top-frequency 데이터 중 빈도가 가장 큰 guard를 구한다.
;; 2. 해당 guard의 top-frequency 요소의 곱을 구한다.


(defn solve-part2
  [guard-sleep-records] ;; input-data를 인자로 받아 처리하도록 변경 필요
  (->> guard-sleep-records
       (sort-by #(->> (last (:top-frequency %))))
       last
       (#(* (:id %) (first (:top-frequency %))))))

(comment
  (solve-part2 guard-sleep-records))


