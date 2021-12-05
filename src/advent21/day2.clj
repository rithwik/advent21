(require '[clojure.string :as str])

(def input
  (->>
   "resources/day2.txt"
   slurp
   str/split-lines
   (map #(str/split % #" "))
   (map (fn [i] [(first i) (read-string (second i))]))))

; part 1
(defn translate [instruction]
  (let [[direction distance] instruction]
    (case direction
      "forward" [distance 0]
      "up" [0 (- distance)]
      "down" [0 distance])))

(->> input
     (map translate)
     (apply map vector)
     (map #(apply + %))
     (apply *)) ; 2073315

; part 2
(defn apply-instruction [state instruction]
  (let [[p d a] state
        [x y] instruction]
    (case x
      "forward" [(+ p y) (+ d (* a y)) a]
      "up" [p d (- a y)]
      "down" [p d (+ a y)])))

(->> (reduce apply-instruction [0 0 0] input)
     (take 2)
     (apply *)) ; 1840311528
