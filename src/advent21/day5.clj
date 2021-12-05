(require '[clojure.string :as str])

(def sample
  (->>
"0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"
str/split-lines
(map #(str/split % #" -> "))
(map (fn [line]
       (->> line
            (map #(str/split % #","))
            (map (fn [p] (map read-string p))))))))

(defn get-sequence [x y]
  (cond
    (= x y) (repeat x)
    (< x y) (range x (inc y))
    (> x y) (range x (dec y) -1)))

(defn points-on-line [line]
  (let [[p1 p2] line
        [x1 y1] p1
        [x2 y2] p2]
    (apply map list [(get-sequence x1 x2) (get-sequence y1 y2)])))

(defn not-diagonal? [line]
  (let [[p1 p2] line
        [x1 y1] p1
        [x2 y2] p2]
    (or (= x1 x2)
        (= y1 y2))))

(->> sample
     ;; (filter not-diagonal?)
     (map points-on-line)
     (apply concat)
     frequencies
     (filter #(> (second %) 1))
     count)

(def input
  (->> "resources/day5.txt"
       slurp
       str/split-lines
       (map #(str/split % #" -> "))
       (map (fn [line]
              (->> line
                   (map #(str/split % #","))
                   (map (fn [p] (map read-string p))))))))

(->> input
     ;; (filter not-diagonal?)
     (map points-on-line)
     (apply concat)
     frequencies
     (filter #(> (second %) 1))
     count)
