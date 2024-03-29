(ns aoc2018_day7)

(def input-data (->> (slurp "data/aoc2018_day7_data")
                      (clojure.string/split-lines)))

(defn parse-to-rule
  [string]
  (->> string
       (re-seq #"Step (\w) .* step (\w)")
       first
       next
       ((fn [[key value]] {:work (keyword key)
                           :next-opt [(keyword value)]}))))

(defn include?
  [item coll]
  (clojure.set/subset? (set (vector item))
                       (set coll)))

(defn get-processing-time
  "각 작업마다 소요되는 시간을 구한다.
   - worker의 수가 1개를 초과하는 경우 문제에서 명시한 알파벳 index + 1 + 60"
  [work number-of-workers]
  (if (< 1 number-of-workers)
    (+ (clojure.string/index-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (name work))
       61)
    1))

(defn update-processing-time
  [number-of-workers rules]
  (map #(assoc % :processing-time
               (get-processing-time (:work %) number-of-workers))
       rules))

(defn get-last-work
  [rules]
  (let [works-without-last (map :work rules)
        all-works (mapcat :next-opts rules)]
    (first (clojure.set/difference (set all-works)
                                   (set works-without-last)))))

(defn add-last-work
  [rules]
  (let [last-work (get-last-work rules)]
    (conj rules {:work last-work :next-opts []})))

(defn get-rules
  [input-data number-of-workers]
  (->> input-data
       (map parse-to-rule)
       (group-by :work) 
       (map (fn [[work grouped]]
              {:work work
               :next-opts (mapcat :next-opt grouped)}))
       add-last-work
       (update-processing-time number-of-workers)))

(defn get-prerequisite-rule
  [rules rule]
  (let [work (:work rule)]
    (->> rules
         (filter #(include? work (:next-opts %)))
         (map :work)
         (hash-map :work work :prerequisites))))

(defn get-prerequisite-rules
  [rules]
  (->> rules
       (map (partial get-prerequisite-rule rules))))


;; --------------
;; * doing 목록 (number-of-worker 만큼의 요소를 갖는다.)
;; worker가 3개인 경우
;; [{:work :B :remain-time 62}
;;  {:work :C :remain-time 63}
;;  {:work :D :remain-time 64}]
;;
;; * processed-time 작업에 걸린 시간을 누적하여 구한다.
;; 
;; 작업 처리
;; - doing 목록 갱신
;; - done 목록 갱신
;; - processed-time 갱신
;;
;; 작업 가능 목록에서 (워커 개수 - doing 요소 개수) 만큼을 가져와서 doing 목록에 추가한다. -> assign works
;; doing 목록 중 작업시간이 최소인 것들을 done 목록으로 옮기고 doing 에서 제거
;; doing 의 남은 작업들의 remain에서 최소 시간만큼 차감
;; processed-time에 최소시간을 더한다.
;; done 길이가 전체 일감 개수와 동일할때 종료한다.

(defn initiate-work-state
  [number-of-workers input-data]
  (let [rules (get-rules input-data number-of-workers)
        prerequisite-rules (get-prerequisite-rules rules)]
    {:rules rules
     :prerequisite-rules prerequisite-rules
     :doing []
     :done []
     :processed-time 0
     :number-of-workers number-of-workers
     :min-time 0
     :min-time-works []}))

(defn get-rules-by-work
  [rules work]
  (filter #(= work (:work %)) rules))

(defn ready?
  "현재 작업 가능여부를 판단한다.
   - 완료, 진행 목록에 없는 것
   - 선행작업이 모두 완료된 것 (선행작업 목록이 없는 것도 포함됨)"
  [done doing prerequisite-rule]
  (let [work (:work prerequisite-rule)
        prerequisites (:prerequisites prerequisite-rule)]
    (and (nil? (some #{work} done))
         (nil? (some #{work} (map :work doing)))
         (clojure.set/subset? (set prerequisites)
                              (set done)))))

(defn get-possible-works
  [{:keys [rules prerequisite-rules doing done number-of-workers]}]
  (->> prerequisite-rules
       (filter #(ready? done doing %))
       (map :work)
       sort
       (take (- number-of-workers (count doing)))
       (mapcat (partial get-rules-by-work rules))
       (map #(hash-map :work (:work %)
                       :remain-time (:processing-time %)))))

(defn assign-works
  [work-state]
  (let [possible-works (get-possible-works work-state)]
    (update work-state :doing concat possible-works)))

(defn update-remain-time
  [min-time rule]
  (assoc rule :remain-time (max 0 (- (:remain-time rule) min-time))))

(defn decrease-min-time
  [min-time doing]
  (->> doing
       (map (partial update-remain-time min-time))))

(defn decrease-remaining-time
  "doing 목록의 작업들의 잔여 작업 시간을 감소시킨.
   - min-time 만큼 doing 의 remain-time에서 차감"
  [{:keys [doing min-time] :as work-state}]
  (let [decreased-remaining-time (decrease-min-time min-time doing)]
    (if (empty? doing)
      work-state
      (assoc work-state :doing decreased-remaining-time))))

(defn increase-processed-time
  [work-state]
  (update work-state :processed-time + (:min-time work-state)))

(defn complete-works
  "작업 완료처리
   - doing 목록 중 작업시간이 최소인 것들을 done 목록으로 옮기고 doing 에서 제거"
  [{:keys [doing min-time-works] :as work-state}]
  (let [removed-min-time-works (remove #(include? (:work %)
                                                  (map :work min-time-works))
                                       doing)]
    (-> work-state
        (update :done concat (map :work min-time-works))
        (assoc :doing removed-min-time-works))))

(defn get-min-time
  [doing]
  (->> doing
       (map :remain-time)
       sort
       first))

(defn get-min-time-works
  [doing]
  (let [min-time (get-min-time doing)]
    (->> doing
         (filter #(= min-time (:remain-time %)))
         (map :work)
         (map #(hash-map :remain-time min-time :work %)))))

(defn update-min-state
  [{:keys [doing] :as work-state}]
  (let [min-time (get-min-time doing)
        min-time-works (get-min-time-works doing)]
    (-> work-state
        (assoc :min-time min-time)
        (assoc :min-time-works min-time-works))))

(defn execute
  [work-state]
  (-> work-state
      assign-works
      update-min-state
      complete-works
      increase-processed-time
      decrease-remaining-time))

(defn done?
  [{:keys [done rules]}]
  (= (count done) (count rules)))

(defn solve-part1
  [input-data number-of-workers]
  (->> input-data
       (initiate-work-state number-of-workers)
       (iterate execute)
       (filter done?)
       first
       :done
       (map name)
       (apply str)))

(defn solve-part2
  [input-data number-of-workers]
  (->> input-data
       (initiate-work-state number-of-workers)
       (iterate execute)
       (filter done?)
       first
       :processed-time))

(comment
  (solve-part1 input-data 1)
  (solve-part2 input-data 5))
