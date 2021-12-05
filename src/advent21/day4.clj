(require '[clojure.string :as str])

(defn transpose [xs]
  (apply map list xs))

(defn move [board marks num]
  (let [newmarks (map (fn [row] (map #(if (= % num) 1 0) row)) board)]
    (->> [(flatten marks) (flatten newmarks)]
         transpose
         (map #(apply + %))
         (partition 5))))

(defn bingo? [marks]
  (let [win? (fn [lst] (some #(= true %) (map (fn [xs] (every? #(= % 1) xs)) lst)))]
    (or (win? marks)
        (win? (transpose marks)))))

(defn answer [board marks num]
  (let [xs (flatten board)
        ys (map #(if (= % 1) 0 1) (flatten marks))]
    (* num (apply + (map #(apply * %) (transpose [xs ys]))))))

(defn play-bingo [board nums]
  (loop [i 0
         marks (repeatedly 5 #(repeat 5 0))]
    (if (= i (count nums))
      (if (bingo? marks) [(dec i) (answer board marks (nth nums (dec i)))] false)
      (if (bingo? marks) [(dec i) (answer board marks (nth nums (dec i)))]
          (recur (inc i) (move board marks (nth nums i)))))))

(def moves
  (as-> "resources/day4.txt" x
    (slurp x)
    (str/split-lines x)
    (take 1 x)
    (first x)
    (str/split x #",")
    (map read-string x)))

(def games
  (->> "resources/day4.txt"
       slurp
       str/split-lines
       (drop 2)
       (filter #(not= 0 (count %)))
       (map str/trim)
       (map #(str/split % #" +"))
       (map #(map read-string %))
       (partition 5 5)))

(->> games
     (map #(play-bingo % moves))
     (sort-by first)
     last)
