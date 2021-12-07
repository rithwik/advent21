(require '[clojure.string :as str])

(def input
  (as-> "resources/day7.txt" x
    (slurp x)
    (str/split x #",")
    (map read-string x)))

;; part 1

(defn fuel-1 [positions proposal]
  (->> (map #(- % proposal) positions)
       (map #(Math/abs %))
       (apply +)))

(defn solution-1 [positions]
  (let [proposals (range (apply min positions) (apply max positions))
        consumptions (map #(fuel-1 positions %) proposals)]
    (->> (apply map vector [proposals consumptions])
         (apply min-key second))))

;; part 2

(defn fuel-2 [positions proposal]
  (->> (map #(- % proposal) positions)
       (map #(Math/abs %))
       (map #(/ (* % (inc %)) 2))
       (apply +)))

(defn solution-2 [positions]
  (let [proposals (range (apply min positions) (apply max positions))
        consumptions (map #(fuel-2 positions %) proposals)]
    (->> (apply map vector [proposals consumptions])
         (apply min-key second))))

(solution-1 input)
(solution-2 input)
