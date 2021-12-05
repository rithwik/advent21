(require '[clojure.string :as str])

(def sample
  (->>
""
str/split-lines))

(def input
  (->> "resources/day6.txt"
       slurp
       str/split-lines))
