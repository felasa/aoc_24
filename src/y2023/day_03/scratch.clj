(ns day-03.scratch)
  
(require '[clojure.string :as s])

(def sol1 (s/split-lines (slurp "3-2-ratios.txt")))
(def sol2 (s/split-lines (slurp "3-2-ratios-fail.txt")))


(take 5 sol1)
(take 5 (reverse sol2))
