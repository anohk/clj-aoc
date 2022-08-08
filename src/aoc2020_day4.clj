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

(defn valid? [target passport]
  (s/valid? target passport))

(defn solve-part-1 [input-data]
  (let [passports (get-passports input-data)]
    (->> passports
         (filter (partial valid? :valid/passport-v1))
         count)))

(comment
  (solve-part-1 input-data))

;; ------
;; part 2
;; 필드 제약 추가

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.

;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.

;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;; pid (Passport ID) - a nine-digit number, including leading zeroes.

;; cid (Country ID) - ignored, missing or not.


;; ----------------------------
;; year (birth/issue/expiration)
;; ----------------------------
(def four-digit-regex #"^\d{4}$")

(defn four-digits? [val] (re-matches #"^\d{4}$" val))

(defn in-range?
  [start end str-val]
  (let [int-val (Integer/parseInt str-val)]
    (and (<= start int-val) (>= end int-val))))


(s/def :constraints/byr (s/and four-digits? #(in-range? 1920 2002 %)))
(s/def :constraints/iyr (s/and four-digits? #(in-range? 2010 2020 %)))
(s/def :constraints/eyr (s/and four-digits? #(in-range? 2020 2030 %)))

(comment
  (s/valid? :constraints/byr "1920")
  (s/valid? :constraints/byr "2002")

  (s/valid? :constraints/iyr "2010")
  (s/valid? :constraints/iyr "2020")
  
  (s/valid? :constraints/eyr "2020")
  (s/valid? :constraints/eyr "2030"))


;; ------
;; height
;; ------
(defmulti in-height-range? (fn [n] (:unit n)))
(defmethod in-height-range? :cm [n]
  (let [height (:height n)]
    (and (<= 150 height) (>= 193 height))))
(defmethod in-height-range? :in [n]
  (let [height (:height n)]
    (and (<= 59 height) (>= 76 height))))
(defmethod in-height-range? "" [n]
  false)

(s/def :constraints/hgt (s/and map? in-height-range?))

(comment
  (s/valid? :constraints/hgt {:height 156 :unit :cm})
  (s/valid? :constraints/hgt {:height 194 :unit :cm})
  (s/valid? :constraints/hgt {:height 56 :unit :in})
  (s/valid? :constraints/hgt {:height 66 :unit :in})
)

;; ----------
;; hair color
;; ----------
(s/def :constraints/hcl #(re-matches #"^#[0-9a-f]{6}$" %))

;; ---------
;; eye color
;; ---------
(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :constraints/ecl (s/and string? #(eye-colors %)))

;; -----------
;; passport id
;; -----------
(s/def :constraints/pid #(re-matches #"[0-9]{9}$" %))



(s/def :passport-v2/byr :constraints/byr)
(s/def :passport-v2/iyr :constraints/iyr)
(s/def :passport-v2/eyr :constraints/eyr)
(s/def :passport-v2/hgt :constraints/hgt)
(s/def :passport-v2/hcl :constraints/hcl)
(s/def :passport-v2/ecl :constraints/ecl)
(s/def :passport-v2/pid :constraints/pid)
(s/def :passport-v2/cid string?)

(s/def :valid/passport-v2
  (s/keys :req-un [:passport-v2/byr
                   :passport-v2/iyr
                   :passport-v2/eyr
                   :passport-v2/hgt
                   :passport-v2/hcl
                   :passport-v2/ecl
                   :passport-v2/pid]
          :opt-un [:passport-v2/cid]))


(defn height-str-to-map
  [height-str]
  (let [height-info (next (re-find #"(\d*)(\w*)" height-str))
        height-str (first height-info)
        unit-str (last height-info)]
    (when (and (not= height-str "") (not= unit-str ""))
      {:height (Integer. height-str)
       :unit (keyword unit-str)})))

(comment
  (next (re-find #"(\d+)(\w*)" "166cm"))
  (height-str-to-map "179cm")
  (height-str-to-map "179in")
  (height-str-to-map "cm")
)

(defn refine-height-info
  "passport 맵에 hgt키가 있는 경우 문자열 값을 맵으로 변환한다."
  [passport]
  (if (:hgt passport)
    (let [height-str (:hgt passport)]
      (assoc passport :hgt (height-str-to-map height-str)))
    passport))

(defn solve-part-2
  [input-data]
  (let [passports (get-passports input-data)]
    (->> passports
         (map refine-height-info)
         (filter (partial valid? :valid/passport-v2))
         count)))


(comment
  (solve-part-2 input-data))
