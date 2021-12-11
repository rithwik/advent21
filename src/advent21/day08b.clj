(require '[clojure.string :as str]
         '[advent21.elves :as elves])

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
              (filter #(elves/coll-subset? (x :1) %) (counts :5))))
      (assoc x :9
             (first
              (filter #(elves/coll-subset? (x :4) %) (counts :6))))
      (assoc x :5
             (first
              (->> (elves/coll-difference (counts :5) (vals x))
                   (filter #( = 1 (count (elves/coll-difference (x :4) %)))))))
      (assoc x :2
             (first (elves/coll-difference (counts :5) (vals x))))
      (assoc x :6
             (->> (elves/coll-difference (counts :6) (vals x))
                  (map #(vector % (elves/coll-union % (encode :1))))
                  (filter #(= 7 (count (second %))))
                  first
                  first))
      (assoc x :0
             (first (elves/coll-difference (counts :6) (vals x)))))))

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
