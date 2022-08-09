(ns aoc2020-day4
  (:require [clojure.spec.alpha :as s]
            [clojure.walk]))


;; ---------
;; 데이터 파싱
;; blank line 으로 개별 여권 구분
;; space or newline 으로 key/value 구분
;; 1. blank line 기준으로 string 분리
;; 2. 개별 string에 newline -> space로 변환
;; 3. space를 기준으로 key/value 쌍을 분리
;; 4. : 를 기준으로 key/value 형태로 변환
;;   [{:birth-year 1999
;;     :issue-year 2020
;;     :expiration-year 2030
;;     :heigt {:height 183 :unit :cm}
;;     :hair-color "#fffffd"
;;     :eye-color "gry"
;;     :passport-id "860033327"
;;     :country-id "147"}
;;    {:birth-year 1894
;;     :issue-year 2015
;;     :height {:height 65 :unit :in}
;;     :passport-id "842032427"}
;;   ...]


(def input-data
  (-> (slurp "data/aoc2020_day4_data")))

(defn str->map [str]
  (let [pair-str (clojure.string/split str #" ")]
    (->> pair-str
         (map #(clojure.string/split % #":"))
         (into {})
         clojure.walk/keywordize-keys)))

(def extended-keywords {:byr :birth-year
                        :iyr :issue-year
                        :eyr :expiration-year
                        :hgt :height
                        :hcl :hair-color
                        :ecl :eye-color
                        :pid :passport-id
                        :cid :country-id})

(defn extend-keyword
  [keywordized-map]
  (->> (for [[k v] keywordized-map] [(extended-keywords k) v])
       (into {})))

(defn get-str-map [input-data]
  (->> input-data
       (clojure.string/split-lines)
       (partition-by #(= "" %))
       (remove #(= "" (first %)))
       (map #(clojure.string/join " " %))
       (map str->map)
       (map extend-keyword)))

(comment
  (get-str-map input-data))


;; -------------------
;; parsing height-info
;; -------------------
(defn height-str->map
  [height-str]
  (let [height-info (next (re-find #"(\d*)(\w*)" height-str))
        height-str (first height-info)
        unit-str (last height-info)]
    (when (and (not= height-str "") (not= unit-str ""))
      {:height (Integer/parseInt height-str)
       :unit (keyword unit-str)})))

(defn refine-height-info
  "passport 맵에 height키가 있는 경우 문자열 값을 맵으로 변환한다."
  [passport]
  (if (:height passport)
    (let [height-str (:height passport)]
      (assoc passport :height (height-str->map height-str)))
    passport))

;; -----------------
;; parsing year-info
;; -----------------
(defn year-str->int
  [year-map]
  (->> (for [[k v] year-map] [k (Integer/parseInt v)])
       (into {})))

(defn refine-year-info
  "passport 맵에 연도와 관련한 키가 있는 경우 문자열 값을 숫자로 변환한다."
  [passport]
  (let [year-map (select-keys passport [:birth-year :issue-year :expiration-year])]
    (if (not= year-map {})
      (merge passport (year-str->int year-map))
      passport)))


(defn get-passports
  [input-data]
  (->> input-data
       get-str-map
       (map refine-height-info)
       (map refine-year-info)))


(comment
  (get-passports input-data))



;; ----------
;; part1
;; 유효한 여권
;;  - 필수 필드를 모두 충족해야한다. country-id는 옵션 필드
;;  - req [:birth-year :issue-year :expiration-year :height :hair-color :eye-color :passport-id]
;;  - opt [:country-id]
;;

(s/def :passport/birth-year int?)
(s/def :passport/issue-year int?)
(s/def :passport/expiration-year int?)
(s/def :passport/height (s/nilable map?))
(s/def :passport/hair-color string?)
(s/def :passport/eye-color string?)
(s/def :passport/passport-id string?)
(s/def :passport/country-id string?)


(s/def :valid/passport-v1
  (s/keys :req-un [:passport/birth-year
                   :passport/issue-year
                   :passport/expiration-year
                   :passport/height
                   :passport/hair-color
                   :passport/eye-color
                   :passport/passport-id]
          :opt-un [:passport/country-id]))

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


;; ----
;; year
;; ----

(defn in-range? [start end val] (and (<= start val) (>= end val)))

(s/def :constraints/birth-year #(in-range? 1920 2002 %))
(s/def :constraints/issue-year #(in-range? 2010 2020 %))
(s/def :constraints/expiration-year #(in-range? 2020 2030 %))

;; ------
;; height
;; ------
(defmulti in-height-range? :unit)

(defmethod in-height-range? :cm [n]
  (let [height (:height n)]
    (and (<= 150 height) (>= 193 height))))

(defmethod in-height-range? :in [n]
  (let [height (:height n)]
    (and (<= 59 height) (>= 76 height))))


(s/def :constraints/height (s/and map? in-height-range?))



(comment
  (s/valid? :constraints/height {:height 156 :unit :cm})
  (s/valid? :constraints/height {:height 194 :unit :cm})
  (s/valid? :constraints/height {:height 56 :unit :in})
  (s/valid? :constraints/height {:height 66 :unit :in})
)

;; ----------
;; hair color
;; ----------
(s/def :constraints/hair-color #(re-matches #"^#[0-9a-f]{6}$" %))

;; ---------
;; eye color
;; ---------
 
(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :constraints/eye-color (s/and string? #(eye-colors %)))

;; -----------
;; passport id
;; -----------
(s/def :constraints/passport-id #(re-matches #"[0-9]{9}$" %))



(s/def :passport-v2/birth-year :constraints/birth-year)
(s/def :passport-v2/issue-year :constraints/issue-year)
(s/def :passport-v2/expiration-year :constraints/expiration-year)
(s/def :passport-v2/height :constraints/height)
(s/def :passport-v2/hair-color :constraints/hair-color)
(s/def :passport-v2/eye-color :constraints/eye-color)
(s/def :passport-v2/passport-id :constraints/passport-id)
(s/def :passport-v2/country-id string?)

(s/def :valid/passport-v2
  (s/keys :req-un [:passport-v2/birth-year
                   :passport-v2/issue-year
                   :passport-v2/expiration-year
                   :passport-v2/height
                   :passport-v2/hair-color
                   :passport-v2/eye-color
                   :passport-v2/passport-id]
          :opt-un [:passport-v2/country-id]))



(defn solve-part-2
  [input-data]
  (let [passports (get-passports input-data)]
    (->> passports
         (filter (partial valid? :valid/passport-v2))
         count)))

(comment
  (solve-part-2 input-data)
)
