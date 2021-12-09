(require '[clojure.string :as str])

;; part 1

(defn get-elem [m i j]
  (nth (nth m i) j))

(defn cartesian-product [l1 l2]
  (for [x l1 y l2] (vector x y)))

(defn get-neighbors [m i j]
  (let [rlo 0
        clo 0
        rhi (dec (count m))
        chi (dec (count (first m)))
        x (get-elem m i j)
        r (get-elem m i (min chi (inc j)))
        l (get-elem m i (max clo (dec j)))
        u (get-elem m (max rlo (dec i)) j)
        d (get-elem m (min rhi (inc i)) j)]
    (cond
      (= i rlo) (cond
                  (= j clo) [r d]
                  (= j chi) [l d]
                  :else [r l d])
      (= i rhi) (cond
                  (= j clo) [r u]
                  (= j chi) [l u]
                  :else [r l u])
      (= j clo) [r u d]
      (= j chi) [l u d]
      :else [r l u d])))

(defn low-point? [m i j]
  (let [x (get-elem m i j)
        ns (get-neighbors m i j)]
    (if (< x (apply min ns)) x)))

(def input
  (as-> "resources/day9.txt" x
    (slurp x)
    (str/split-lines x)
    (map #(str/split % #"") x)
    (map #(map read-string %) x)))

(->> (cartesian-product (range (count input)) (range (count (first input))))
     (map #(apply (partial low-point? input) %))
     (remove nil?)
     (map inc)
     (apply +))

;; part 2

(defn get-neighbors-addresses [m i j]
  (let [rlo 0
        clo 0
        rhi (dec (count m))
        chi (dec (count (first m)))
        r [i (min chi (inc j))]
        l [i (max clo (dec j))]
        u [(max rlo (dec i)) j]
        d [(min rhi (inc i)) j]]
    (cond
      (= i rlo) (cond
                  (= j clo) [r d]
                  (= j chi) [l d]
                  :else [r l d])
      (= i rhi) (cond
                  (= j clo) [r u]
                  (= j chi) [l u]
                  :else [r l u])
      (= j clo) [r u d]
      (= j chi) [l u d]
      :else [r l u d])))

(defn get-heigher-neighbors-addresses [m i j]
  (let [x (get-elem m i j)
        ns (get-neighbors-addresses m i j)]
    (filter #(< x (apply (partial get-elem m) %)) ns)))

(defn get-heigher-neighbors-addresses-without-9s [m i j]
  (->> (get-heigher-neighbors-addresses m i j)
       (remove #(= 9 (apply (partial get-elem m) %)))))

(defn get-basin [m i j]
  (if (low-point? m i j)
    (loop [xy [[i j]]
           hns (get-heigher-neighbors-addresses-without-9s m i j)]
      (if (empty? hns) (distinct xy)
          (recur
           (concat hns xy)
           (->> hns
                (mapcat #(apply (partial get-heigher-neighbors-addresses-without-9s m) %))))))))

(->> (cartesian-product (range (count input)) (range (count (first input))))
     (map #(apply (partial get-basin input) %))
     (remove nil?)
     (map count)
     (sort)
     (reverse)
     (take 3)
     (apply *))
