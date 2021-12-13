(require '[advent21.elves :as elves]
         '[clojure.string :as str])

(defn get-dots [data]
  (->> data
     (partition-by #(= "" %))
     (first)
     (map #(elves/str-split #"," %))
     (map reverse)
     (mapv (fn [xy] (mapv elves/str->int xy)))))

(defn get-folds [data]
  (->> data
     (partition-by #(= "" %))
     (drop 2)
     (first)
     (mapv #(elves/str-split #" " %))
     (mapv #(nth % 2))
     (mapv #(elves/str-split #"=" %))
     (map (fn [[d q]] [(if (= d "x") :v :h) (elves/str->int q)]))))

(defn get-grid [data]
  (let [dots (get-dots data)
        nrows (inc (apply max (map first dots)))
        ncols (inc (apply max (map second dots)))
        grid (vec (repeatedly nrows #(vec (repeat ncols false))))]
    (reduce #(assoc-in %1 %2 true) grid dots)))

(defn split [grid axis position]
  (cond
    (= axis :h)
    [(take position grid)
     (drop (inc position) grid)]
    (= axis :v)
    [(map #(take position %) grid)
     (map #(drop (inc position) %) grid)]))

(defn flip [grid axis]
  (cond
    (= axis :h) (reverse grid)
    (= axis :v) (mapv reverse grid)))

(defn overlay [g1 g2]
  (mapv (fn [x1 x2] (mapv #(or %1 %2) x1 x2)) g1 g2))

(defn pad [grid direction n]
  (let [nrows (count grid) ncols (count (first grid))]
    (cond
    (= direction :u) (concat (vec (repeatedly n #(vec (repeat ncols false)))) grid)
    (= direction :d) (concat grid (vec (repeatedly n #(vec (repeat ncols false)))))
    (= direction :l) (mapv (fn [r] (vec (concat (vec (repeat n false)) r))) grid)
    (= direction :r) (mapv (fn [r] (vec (concat r (vec (repeat n false))))) grid))))

(defn fold [grid axis position]
  (let [[g1 g2] (split grid axis position)
        g2 (flip g2 axis)]
    (cond
      (= axis :h)
      (let [diff (- (count g1) (count g2))]
        (cond
          (pos? diff) (overlay g1 (pad g2 :u diff))
          (= 0 diff) (overlay g1 g2)
          :else (overlay (pad g1 :u (- diff)) g2)))
      (= axis :v)
      (let [diff (- (count (first  g1)) (count (first g2)))]
        (cond
          (pos? diff) (overlay g1 (pad g2 :l diff))
          (= 0 diff) (overlay g1 g2)
          :else (overlay (pad g1 :l (- diff)) g2))))))

(defn folder [grid instruction]
  (let [[axis position] instruction]
    (fold grid axis position)))

(def input
  (->> "resources/day13.txt"
       (elves/read-lines)))

;; part 1
(->> input
     get-folds
     first
     vector
     (reduce #(folder %1 %2) (get-grid input))
     (map #(filterv true? %))
     (map count)
     (apply +))

;; part 2
(->> input
     get-folds
     (reduce #(folder %1 %2) (get-grid input))
     (mapv (fn [r] (mapv #(if (true? %) "X" ".") r)))
     (mapv #(apply str %)))
