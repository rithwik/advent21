(require '[clojure.string :as str])

(def input
  (->>
   "resources/day3.txt"
   slurp
   str/split-lines))

(defn get-gamma-nth-digit [xs n]
  (let [f (->> xs
         (map #(str/split % #""))
         (map #(nth % n))
         frequencies)]
    (if (< (get f "0") (get f "1")) "1" "0")))

(defn get-gamma [xs]
  (let [n (count (first xs))]
    (apply str (map #(get-gamma-nth-digit xs %) (range n)))))

(defn get-epsilon-nth-digit [xs n]
  (let [f (->> xs
         (map #(str/split % #""))
         (map #(nth % n))
         frequencies)]
    (if (< (get f "0") (get f "1")) "0" "1")))

(defn get-epsilon [xs]
  (let [n (count (first xs))]
    (apply str (map #(get-epsilon-nth-digit xs %) (range n)))))

;; Integer/parseInt num base
(defn binary-to-decimal [n]
  (as-> n x
    (str/split x #"")
    (map read-string x)
    (map #(* %1 %2) (map #(Math/pow 2 %) (reverse (range (count n)))) x)
    (apply + x)))

(as-> input xs
  (* (binary-to-decimal (get-gamma xs))
     (binary-to-decimal (get-epsilon xs))))

; part 2

(defn get-o2-nth-digit [xs n]
  (let [f (->> xs
         (map #(str/split % #""))
         (map #(nth % n))
         frequencies)]
    (if (> (get f "0" 0) (get f "1" 0)) "0" "1")))

(defn get-o2-rating [xs]
  (let [n (count (first xs))]
    (loop [i 0 candidates xs]
    (if (and (< i n) (vector? candidates) (> (count candidates) 1))
      (recur (inc i)
             (filterv #(= (str (nth % i))
                          (get-o2-nth-digit candidates i)) candidates))
      (first candidates)))))

(defn get-co2-nth-digit [xs n]
  (let [f (->> xs
         (map #(str/split % #""))
         (map #(nth % n))
         frequencies)]
    (if (< (get f "1" 0) (get f "0" 0)) "1" "0")))

(defn get-co2-rating [xs]
  (let [n (count (first xs))]
    (loop [i 0 candidates xs]
    (if (and (< i n) (vector? candidates) (> (count candidates) 1))
      (recur (inc i)
             (filterv #(= (str (nth % i))
                          (get-co2-nth-digit candidates i)) candidates))
      (first candidates)))))

(as-> input xs
  (* (binary-to-decimal (get-o2-rating xs))
     (binary-to-decimal (get-co2-rating xs))))
