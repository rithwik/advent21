(require '[clojure.string :as str]
         '[clojure.set :as set])

(def input
  (as-> "resources/day8.txt" x
    (slurp x)
    (str/split-lines x)
    (map #(str/split % #"\s\|\s") x)))

;; part 1

(as-> input x
  (map second x)
  (mapcat #(str/split % #"\s") x)
  (map count x)
  (filter #(or (= % 2) (= % 4) (= % 3) (= % 7)) x)
  (count x))

;; part 2

(defn possible [string]
  (let [n (count string)]
    (cond
      (= n 2) 1
      (= n 4) 4
      (= n 3) 7
      (= n 7) 8
      (= n 5) #{2 3 5}
      (= n 6) #{0 6 9})))

(defn possibilities [strings]
  (let [ss (str/split strings #"\s")]
    (zipmap ss (map possible ss))))

(defn str-subset? [s1 s2]
  (let [s1 (set s1) s2 (set s2)]
    (set/subset? s1 s2)))

(defn str-union [s1 s2]
  (let [s1 (set s1) s2 (set s2)]
    (set/union s1 s2)))

(defn get3 [possibilities]
  (let [one (first (keys (filter #(= 1 (second %)) possibilities)))
        possible3 (keys (filter #(= #{2 3 5} (second %)) possibilities))]
    (first (filter #(str-subset? one %) possible3))))

(defn get9 [possibilities]
  (let [four (first (keys (filter #(= 4 (second %)) possibilities)))
        possible9 (keys (filter #(= #{0 6 9} (second %)) possibilities))]
    (first (filter #(str-subset? four %) possible9))))

(defn get5 [possibilities]
  (let [four (first (keys (filter #(= 4 (second %)) possibilities)))
        possible5 (keys (filter #(= #{2 3 5} (second %)) possibilities))]
    (first (filter #(= 1 (count (set/difference (set four) (set %)))) possible5))))

(defn get2 [possibilities]
  (first (keys (filter #(= #{2 3 5} (second %)) possibilities))))

(defn get6 [possibilities]
  (let [one (first (keys (filter #(= 1 (second %)) possibilities)))
        eight (first (keys (filter #(= 8 (second %)) possibilities)))
        possible6 (keys (filter #(= #{0 6 9} (second %)) possibilities))]
    (apply str (first (first (filter #(= (set eight) (second %)) (map #(vector % (str-union one %)) possible6)))))))

(defn get0 [possibilities]
  (first (keys (filter #(= #{0 6 9} (second %)) possibilities))))

(defn decoded-map [string]
  (let [ps (possibilities string)]
    (as-> ps x
      (assoc x (get3 x) 3)
      (assoc x (get9 x) 9)
      (assoc x (get5 x) 5)
      (assoc x (get2 x) 2)
      (assoc x (get6 x) 6)
      (assoc x (get0 x) 0)
      (zipmap (map set (keys x)) (vals x)))))

(defn decode [string]
  (let [[obs reading] string
        decodemap (decoded-map obs)]
    (as-> reading x
      (str/split x #"\s")
      (map set x)
      (map #(get decodemap %) x)
      (apply str x))))

(->> input
     (map decode)
     (map #(Integer/parseInt %))
     (apply +))

