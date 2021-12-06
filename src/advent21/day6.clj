(require '[clojure.string :as str])

(def input
  (as-> "resources/day6.txt" x
    (slurp x)
    (str/split x #",")
    (map read-string x)))

;; part 1

(defn transition-swarm [xs]
  (flatten (map #(if (= % 0) [6 8] (dec %)) xs)))

(->> input
     (iterate transition-swarm)
     (take 81)
     last
     count)

;; part 2

(defn transition-state [state]
  {0 (get state 1 0)
   1 (get state 2 0)
   2 (get state 3 0)
   3 (get state 4 0) 
   4 (get state 5 0) 
   5 (get state 6 0) 
   6 (+ (get state 7 0) (get state 0 0))
   7 (get state 8 0)
   8 (get state 0 0)})

(->> input
     frequencies
     (iterate transition-state)
     (take 257)
     last
     (map second)
     (apply +))
