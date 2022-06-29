(ns aoc2020_day8)

(def input-data (->> (slurp "data/aoc2020_day8_data")
                     (clojure.string/split-lines)))
;; ----------------
;; 필요한 데이터

;; {:next-idx 0
;;  :acc 0
;;  :visited []
;;  :instructions [{:idx 0
;;                  :operation "jmp"
;;                  :argument 10}
;;                 {:idx 1
;;                  :operation "acc"
;;                  :argument -5}
;;                  ...]

;; operation을 실행할 때 마다 idx를 visited 에 추가한다. 이때 이미 들어있으면 실행을 멈추고 값을 반환한다.


(defn get-argument
  [operator value-str]
  (let [value (Integer/parseInt value-str)]
    (if (= operator "-")
      (- value)
      value)))

(defn convert-to-map
  [[operation operator-str value-str]]
  (hash-map :operation operation
            :argument (get-argument operator-str value-str)))

(defn parse-boot-code
  [instruction-str]
  (->> (re-seq #"(\w+) ([\-\+])(\d+)" instruction-str)
       first
       next
       convert-to-map))

(defn create-instructions
  [input-data]
  (->> input-data 
       (map parse-boot-code)
       (map-indexed (fn [idx instruction]
                      (assoc instruction :idx idx)))))

(defn initiate-guide
  [input-data]
  (let [instructions {:next-idx 0
                      :acc 0
                      :visited [] }]
    (->> input-data
         create-instructions
         (assoc instructions :instructions))))

(defn execute
  [guide]
  (let [next-idx (:next-idx guide)
        guide (update guide :visited conj next-idx)
        instructions (:instructions guide)
        instruction (first (filter #(= next-idx (:idx %)) instructions))
        operation (:operation instruction)
        argument (:argument instruction)
        idx (:idx instruction)]
    (->> (case operation
           "jmp" (update-info guide 0 (+ idx argument))
           "nop" (update-info guide 0 (inc idx))
           "acc" (update-info guide argument (inc idx))))))

(defn update-guide
  [guide increment next-idx]
  (-> guide
      (assoc :next-idx next-idx)
      (update :acc + increment)))

(defn visited?
  [guide]
  (some (set (vector (:next-idx guide))) (:visited guide)))

(defn solve-part1
  [input-data]
  (let [guide (initiate-guide input-data)]
    (reduce (fn [guide _]
              (if (visited? guide)
                (reduced (:acc guide))
                (execute guide)))
            guide
            (range))))

(comment 
  (solve-part1 input-data))

