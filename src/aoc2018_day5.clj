(ns aoc2018_day5)

(def input-data (->> (slurp "data/aoc2018_day5_data")))


;; ----------------------------------------------------------------------
;; part1
;;
;; 주어진 문자열에서 근접 문자 끼리 비교
;; 같은 종류의 소문자, 대문자의 경우 제거하여 결과 문자열을 리턴
;; aA 또는 Aa -> ""
;; AA aa 는 변화없음
;;
;;
;; * 두 문자에 대해 반응 여부를 리턴하는 함수
;;   - 두 문자를 비교하여 같은 종류의 소문자, 대문자인 경우 반응
;;   - 그 외의 조합은 반응하지 않음
;;


(defn reactive?
  [unit1 unit2]
  (and (not= unit1 unit2)
       (= (clojure.string/upper-case unit1)
          (clojure.string/upper-case unit2))))

(defn acc-polymer
  [polymer unit]
  (if (reactive? (last polymer) unit)
    (pop polymer)
    (conj polymer unit)))


(defn solve-part1
  [input-data]
  (->> input-data
       (reduce acc-polymer [""])
       (apply str)
       count))

(solve-part1 input-data)


;; ----------------------------------------------------------------------
;; part2
;;
;; a/A - z/Z 하나의 알파벳 (대소문자포함)을 모두 제거한 채로 part1 수행
;; 결과가 가장 짧은 경우를 찾아 결과 문자열을 리턴
;; dabAcCaCBAcCcaDA 의 경우, a/A 제거보다 c/C 를 제거한 경우의 길이가 더 짧음


(def reduction-types
  [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z])


(defn remove-unit
  [unit-lower unit-upper units]
  (-> units
      (clojure.string/replace unit-lower "")
      (clojure.string/replace unit-upper "")))


(defn remove-target
  [target units]
  (let [unit-lower (name target)
        unit-upper (clojure.string/upper-case unit-lower)]
    (remove-unit unit-lower unit-upper units)))


(defn solve-part2
  [reduction-types units]
  (->> reduction-types
       (map #(remove-target (name %) units))
       (map solve-part1)
       sort
       first))

(remove-targets reduction-types input-data)


