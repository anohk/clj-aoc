(ns aoc2020-day4
  (:require [clojure.spec.alpha :as s]
            [clojure.walk]))

;; ----------
;; part1
;; 유효한 여권
;;  - 필수 필드를 모두 충족해야한다. cid는 옵션 필드
;;  - req [:byr :iyr :eyr :hgt :hcl :ecl :pid]
;;  - opt [:cid]
;;

;; ---------
;; 데이터 파싱
;; blank line 으로 개별 여권 구분
;; space or newline 으로 key/value 구분
;; 1. blank line 기준으로 string 분리
;; 2. 개별 string에 newline -> space로 변환
;; 3. space를 기준으로 key/value 쌍을 분리
;; 4. : 를 기준으로 key/value 형태로 변환
;;   [{:byr "1999"
;;    :iyr "2020"
;;    :eyr "2030"
;;    :hgt "183cm"
;;    :hcl "#fffffd"
;;    :ecl "gry"
;;   :pid "860033327"
;;    :cid "147"}
;;   {:byr "1894"
;;    :iyr "2015"
;;    :eyr "2025"
;;    :hgt "165cm"
;;    :hcl "#ffffff"
;;    :ecl "blk"
;;    :pid "842032427"}
;;   ...]



(def input-data
  (-> (slurp "data/aoc2020_day4_data")))

(defn str-to-map [str]
  (let [pair-str (clojure.string/split str #" ")]
    (->> pair-str
         (map #(clojure.string/split % #":"))
         (into {})
         clojure.walk/keywordize-keys)))

(defn get-passports [input-data]
  (->> input-data
       (clojure.string/split-lines)
       (partition-by #(= "" %))
       (remove #(= "" (first %)))
       (map #(clojure.string/join " " %))
       (map str-to-map)))

(comment
  (get-passports input-data))


(s/def :passport/byr string?)
(s/def :passport/iyr string?)
(s/def :passport/eyr string?)
(s/def :passport/hgt string?)
(s/def :passport/hcl string?)
(s/def :passport/ecl string?)
(s/def :passport/pid string?)
(s/def :passport/cid string?)

(s/def :valid/passport-v1
  (s/keys :req-un [:passport/byr
                   :passport/iyr
                   :passport/eyr
                   :passport/hgt
                   :passport/hcl
                   :passport/ecl
                   :passport/pid]
          :opt-un [:passport/cid]))

(defn valid? [passport]
  (s/valid? :valid/passport-v1 passport))

(defn solve-part-1 [input-data]
  (let [passports (get-passports input-data)]
    (->> passports
         (filter valid?)
         count)))

(comment
  (solve-part-1 input-data))

