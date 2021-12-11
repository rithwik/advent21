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

(defn subset? [s1 s2]
  (let [s1 (set s1) s2 (set s2)]
    (set/subset? s1 s2)))

(defn union [s1 s2]
  (let [s1 (set s1) s2 (set s2)]
    (set/union s1 s2)))

(defn difference [s1 s2]
  (let [s1 (set s1) s2 (set s2)]
    (set/difference s1 s2)))

(defn get-counts [obs]
  (let [counts (group-by count obs)]
    (zipmap
     (map #(keyword (str %)) (keys counts))
     (vals counts))))

(defn get-encoding [observations]
  (let [obs (str/split observations #"\s")
        counts (get-counts obs)
        encode {:1 (first (counts :2))
                :4 (first (counts :4))
                :7 (first (counts :3))
                :8 (first (counts :7))}]
    (as-> encode x
      (assoc x :3
             (first
              (filter #(subset? (x :1) %) (counts :5))))
      (assoc x :9
             (first
              (filter #(subset? (x :4) %) (counts :6))))
      (assoc x :5
             (first
              (->> (difference (counts :5) (vals x))
                   (filter #( = 1 (count (difference (x :4) %)))))))
      (assoc x :2
             (first (difference (counts :5) (vals x))))
      (assoc x :6
             (->> (difference (counts :6) (vals x))
                  (map #(vector % (union % (encode :1))))
                  (filter #(= 7 (count (second %))))
                  first
                  first))
      (assoc x :0
             (first (difference (counts :6) (vals x)))))))

(defn decode [string]
  (let [[obs reading] string
        encoding (get-encoding obs)
        decoding (zipmap (map set (vals encoding)) (map name (keys encoding)))]
    (as-> reading x
      (str/split x #"\s")
      (map set x)
      (map #(get decoding %) x)
      (apply str x))))

(->> input
     (map decode)
     (map #(Integer/parseInt %))
     (apply +))
