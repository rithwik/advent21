(require '[clojure.string :as str]
         '[advent21.elves :as elves])

(def input
  (->> "resources/day11.txt"
      (elves/read-lines)
      (mapv elves/str->chars)
      (mapv #(mapv elves/str->int %))))

(defn flash [m [i j]]
  (let [m1 (elves/inc-neighbors m [i j])
        m2 (elves/reset-element m1 [i j])]
    m2))

(defn step [grid]
  (let [m (:m grid)
        f (:flashes grid)
        nrows (count m)
        ncols (count (first m))
        xys (elves/cartesian-product (range nrows) (range ncols))
        m1 (reduce #(elves/inc-element %1 %2) m xys)]
    (loop [m m1
           flashxys (filter #(> (get-in m %) 9) xys)
           accumulated flashxys]
      (if (empty? flashxys)
        {:m (->> accumulated
              (remove empty?)
              (reduce #(elves/reset-element %1 %2) m))
         :flashes (->> accumulated
                       (remove empty?)
                       (count)
                       (conj f))}
        (let [m1 (reduce #(flash %1 %2) m flashxys)
              m2 (reduce #(elves/reset-element %1 %2) m1 flashxys)
              newflashxys (filter #(> (get-in m2 %) 9) xys)]
          (recur m2 newflashxys (concat accumulated newflashxys)))))))

;; part 1
(->> (iterate step {:m input :flashes [0]})
     (take 101)
     last
     :flashes
     (apply +))

;; part 2
(->> (iterate step {:m input :flashes [0]})
     (take-while #(not= 100 (peek (:flashes %))))
     last
     :flashes
     count)
