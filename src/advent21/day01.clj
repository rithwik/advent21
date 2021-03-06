(require '[clojure.string :as str])

(def input
  (->> "resources/day1.txt"
       slurp
       str/split-lines
       (map read-string)))

;; (defn count-increasing [xs]
;;   (->> (list xs (rest xs))
;;        (apply map list)
;;        (map #(apply - %))
;;        (filter #(< % 0))
;;        count))

(defn count-increasing [xs]
  (->> xs
       (partition 2 1)
       (filter (fn [[x y]] (< x y)))
       count))

(count-increasing input) ; 1228

(->> input
     (partition 3 1)
     (map #(apply + %))
     count-increasing) ; 1257
