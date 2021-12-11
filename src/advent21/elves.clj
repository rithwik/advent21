(ns advent21.elves
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn read-lines [file]
  (str/split-lines (slurp file)))

(defn str->chars [s]
  (str/split s #""))

(defn str-split [re s]
  (str/split s re))

(defn str->int
  ([s]
   (str->int 10 s))
  ([base s]
   (Integer/parseInt s base)))

(defn zip [& colls]
  (apply mapv vector colls))

(defn transpose [m]
  (apply mapv vector m))

(defn enumerate
  ([coll]
   (enumerate coll 0))
  ([coll start]
   (zip (drop start (range)) coll)))

(defn vec-remove [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn coll-contains? [coll value]
  (some #(= % value) coll))

(defn coll-subset? [coll1 coll2]
  (let [s1 (set coll1) s2 (set coll2)]
    (set/subset? s1 s2)))

(defn coll-union [coll1 coll2]
  (let [s1 (set coll1) s2 (set coll2)]
    (set/union s1 s2)))

(defn coll-difference [coll1 coll2]
  (let [s1 (set coll1) s2 (set coll2)]
    (set/difference s1 s2)))

(defn cartesian-product [l1 l2]
  (for [x l1 y l2] (vector x y)))

(defn neighbor-indices [m [i j]]
  (let [rows (count m) cols (count (first m))]
    (->> (cartesian-product [0 1 -1] [0 1 -1])
       (remove (fn [[x y]] (and (= x 0) (= y 0))))
       (map (fn [[x y]] [(+ i x) (+ j y)]))
       (filter (fn [[x y]] (and (>= x 0)
                               (>= y 0)
                               (< x rows)
                               (< y cols)))))))

(defn inc-element [m [i j]]
  (assoc-in m [i j] (inc (get-in m [i j]))))

(defn reset-element
  ([m [i j]]
   (reset-element m [i j] 0))
  ([m [i j] reset-value]
   (assoc-in m [i j] reset-value)))

(defn inc-neighbors [m [i j]]
  (let [ns (neighbor-indices m [i j])]
    (reduce #(inc-element %1 %2) m ns)))
