(ns day-10
  (:require [clojure.string :as s]))

(def example-input 
  "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defn to-int-arr [input]
  (->> input 
       (s/split-lines)
       (map #(s/split  % #""))
       (mapv #(mapv parse-long %))))

(defn up [[row col]]
  [(dec row) col])
(defn right [[row col]]
  [row (inc col)])
(defn down [[row col]]
  [(inc row) col])
(defn left [[row col]]
  [row (dec col)])

(defn access-array [array [row col]]
  (if-let [v-row (get array row)]
    (get v-row col)
    nil))

(defn terminal-positions 
  [data position]
  (if-let [digit (access-array data position)]
     (cond 
       (= digit 9) [position]
       :else (let [neighbors (filter #(= ((partial access-array data) %) (inc digit)) 
                                     (map #(% position) [up right down left]))]
               (reduce into #{} (map #(terminal-positions data %) neighbors))))
     nil))

(defn starting-postitions [data]
  (for [row (range 0 (count data))
        col (range 0 (count (get data 0)))
        :when (= 0 (access-array data [row col]))]
    [row col]))

(comment 
  (starting-postitions (to-int-arr example-input)))

(defn solution-1 [input]
  (let [data (to-int-arr input)
        mem-positions (memoize (partial terminal-positions data))]
    (->> data
        starting-postitions
        (map mem-positions) 
        (map count)
        (reduce +))))

(comment 
  (to-int-arr example-input)
  (access-array (to-int-arr example-input) [7 7])
  (solution-1 example-input) ;; 36
  (solution-1 (slurp "resources/input/input_10"))) ;; 760

(defn s1 []
  (solution-1 (slurp "resources/input/input_10")))
  