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
       
(comment 
  (read-input example-input))

(defn tick 
  "Moves by times applying wrap"
  [ncols nrows times [start-col start-row] [dcol drow]]
  (let [mod-col (rem (+ start-col (* times dcol)) ncols)
        mod-row (rem (+ start-row (* times drow)) nrows)]
    [(if (>= mod-col 0) mod-col (+ ncols mod-col))
     (if (>= mod-row 0) mod-row (+ nrows mod-row))]))

(defn cuadrant 
  "Maps position to quadrant"
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
  (solution-1 101 103 (slurp "resources/input/2024/input_14"))) ;; 208437768

(defn s2 []
  (solution-1 101 103 (slurp "resources/input/2024/input_14")))

;;PART 2

(defn positions-at [ticks] 
  (let [input (slurp "resources/input/2024/input_14")
        ncols 101 nrows 103]
    (as-> input x (read-input x)
         (map #((partial tick ncols nrows ticks) (:start-pos %) (:velocity %)) x)
         (group-by identity x)
         (update-vals x (constantly 1)))))

(defn print-positions
  [positions]
  (let [filled "X"
        empty "."]
    (loop [row 0
           col 0]
      (if (>= row 103) nil
        (if (>= col 101) (do (println "") (flush) (recur (inc row) 0))
          (if (> (get positions [col row] 0) 0) 
            (do (print filled) (recur row (inc col)))
            (do (print empty) (recur row (inc col))))))))) 

;; No idea how is one supposed to figure it out but i've seen how the solutions looks
;; so assuming a sensible solution would be to look for a compact arrangement of robots
;; ill compute the 'spread' of a given arrangement and search for an arrangement with the
;; least spread
(defn centroid 
  [positions]
  (let [n (reduce + (vals positions))
        [Sx Sy] (reduce (fn [[pc pr] [col row]] [(+ pc col) (+ pr row)]) [0 0]   
                        (keys positions))]
    [(double (/ Sx n)) (double (/ Sy n))]))

(comment 
  (centroid (positions-at 5)))

(defn deviations
  [centroid positions]
  (let [[cc cr] centroid]
    (map (fn [kv] (* (val kv) (+ (abs (- cc ((key kv) 0))) (abs (- cr ((key kv) 1)))))) 
         positions)))

(comment 
  (deviations (centroid (positions-at 0)) (positions-at 0)))

(defn spread 
  [positions]
  (let [centroid (centroid positions)
        deviations (deviations centroid positions)
        n (reduce + (vals positions))]
    (/ (reduce + deviations) n)))

(comment 
  (spread (positions-at 0))
  (reduce (fn [l r] (if (< (:spread r) (:spread l)) r l)) 
    (map #(assoc {} :idx % :spread (spread (positions-at %))) (range 7000 7500))))

(defn solution-2 []
  (let [min-spread 
        (reduce (fn [l r] (if (< (:spread r) (:spread l)) r l)) 
                (map #(assoc {} :idx % :spread (spread (positions-at %))) 
                     (range 0 10000)))]
    (:idx min-spread)))

(comment 
  (solution-2) ;; 7492
  (print-positions (positions-at 7492)))

(defn s2 []
  (let [idx (solution-2)]
    (print-positions (positions-at idx))
    idx))

