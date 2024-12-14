(ns day-14
  (:require [clojure.string :as s]))

(def example-input 
  "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

(defn codify-robot 
  [[_ col row _ dcol drow]]
  {:start-pos [(parse-long col) (parse-long row)]
   :velocity [(parse-long dcol) (parse-long drow)]})

(defn read-input [s]
 (->> s s/split-lines
      (map #(s/split % #" |,|="))
      (map codify-robot)))
       
(defn tick 
  [ncols nrows times [start-col start-row] [dcol drow]]
  (let [mod-col (rem (+ start-col (* times dcol)) ncols)
        mod-row (rem (+ start-row (* times drow)) nrows)]
    [(if (>= mod-col 0) mod-col (+ ncols mod-col))
     (if (>= mod-row 0) mod-row (+ nrows mod-row))]))

(defn cuadrant 
  [ncols nrows [col row]]
  (case [(< col (quot ncols 2)) (< row (quot nrows 2))]
    [true   true] "TL"
    [true  false] "BL"
    [false  true] "TR"
    [false false] "BR"))
  
(defn solution-1 
  [ncols nrows input]
  (->
    (->> input read-input
         (map #((partial tick ncols nrows 100) (:start-pos %) (:velocity %)))
         (filter (fn [v] (and (not= (v 0) (quot ncols 2)) (not= (v 1) (quot nrows 2)))))
         (group-by (fn [v] (cuadrant ncols nrows v))))
    (update-vals count)
    (vals)
    ((partial reduce *))))

(comment 
  (solution-1 11 7 example-input) ;; 12
  (solution-1 101 103 (slurp "resources/input/input_14"))) ;; 208437768

(defn s2 []
  (solution-1 101 103 (slurp "resources/input/input_14")))

