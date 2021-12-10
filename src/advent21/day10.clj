(require '[clojure.string :as str])

(def input
  (as-> "resources/day10.txt" x
    (slurp x)
    (str/split-lines x)))

(def opens (str/split "([{<" #""))
(def closes (str/split ")]}>" #""))
(def flips {"(" ")" "[" "]" "{" "}" "<" ">"})

(defn parse [string]
  (loop [monitor []
         lst (str/split string #"")]
    (if (empty? monitor)
      (if (.contains opens (first lst))
        (recur (vector (first lst)) (rest lst))
        {:monitor monitor :error (first lst)})
      (if (.contains opens (first lst))
        (recur (conj monitor (first lst)) (rest lst))
        (if (= (first lst) (get flips (peek monitor)))
          (recur (pop monitor) (rest lst))
          {:monitor monitor :error (first lst)})))))

;; part 1

(def scores-1 {")" 3 "]" 57 "}" 1197 ">" 25137})

(->> input
     (map parse)
     (map :error)
     (remove nil?)
     (map #(get scores-1 %))
     (apply +))

;; part 2

(def scores-2 {")" 1 "]" 2 "}" 3 ">" 4})

(defn middle [xs]
  (let [n (count xs)  m (/ (dec n) 2)]
    (nth xs m)))

(->> input
     (map parse)
     (filter #(nil? (:error %)))
     (map :monitor)
     (map reverse)
     (map (fn [x] (map #(get flips %) x)))
     (map (fn [x] (map #(get scores-2 %) x)))
     (map (fn [x] (reduce #(+ %2 (* %1 5)) x)))
     (sort)
     (middle))
