(ns day-08
  (:require [clojure.string :as s]))
(def example-input 
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(defn antenna-frequencies
 "Find the symbols used for antennae"
 [s] 
 (set (filter (complement #{\newline \.}) s))) 

(defn shape-input
  [s]
  (s/split-lines s))

(defn antenna-positions
  "Returns pairs f antenna symbol and position"
  [s]
  (let [a-freq (antenna-frequencies s)
        data (shape-input s)]
     (loop [row 0 col 0 result nil]
       (if (>= row (count data)) result
         (if (>= col (count (data 0)))
           (recur (inc row) 0 result)
           (let [c (get (data row) col)]
             (if (a-freq c)
               (recur row (inc col) (conj result [c [row col]]))
               (recur row (inc col) result))))))))   

(defn anti-nodes
  "Gets the antinode position for a given pair"
  [coord1 coord2]
  (let [r1 (coord1 0) c1 (coord1 1)
        r2 (coord2 0) c2 (coord2 1)]
    [[(+ r1 (- r1 r2)) (+ c1 (- c1 c2))]
     [(+ r2 (- r2 r1)) (+ c2 (- c2 c1))]]))

(defn all-pairs
  "For a collection of coordinates >= 2 return all the distinct pairings"
  [coords]
  (loop [to-do coords
         result nil]
    (if (< (count to-do) 2) result
      (let [R (rest to-do)]
        (recur R
               (apply (partial conj result)
                      (map (partial vector (first to-do)) R))))))) 

(defn solution-1 [s] 
  (let [data (shape-input s)
        nrows (count data)
        ncols (count (data 0))] 
    (as-> s x
          (antenna-positions x)
          (group-by first x)
          (update-vals x (fn [coll] (mapv second coll)))
          (map (fn [kv] (all-pairs (val kv))) x)
          (reduce into x)
          (map (fn [pair] (anti-nodes (pair 0) (pair 1))) x)
          (reduce concat x)
          (filter (fn [v] (and (<= 0 (get v 0) (dec nrows)) (<= 0 (get v 1) (dec ncols)))) x)
          (distinct x)
          (count x))))
          
(comment 
  (solution-1 example-input) ;; 14
  (solution-1 (slurp "resources/input/2024/input_08"))) ;; 364

(defn s1 []
  (solution-1 (slurp "resources/input/2024/input_08")))

;; PART 2

(defn in-bounds?
  "check if coord is in bounds [n-rows n-cols]"
  [bounds coord]
  (and (<= 0 (coord 0) (dec (bounds 0)))
       (<= 0 (coord 1) (dec (bounds 1)))))

(defn anti-nodes2
  "Gets the antinode position for a given pair"
  [bounds coord1 coord2]
  (let [r1 (coord1 0) c1 (coord1 1)
        r2 (coord2 0) c2 (coord2 1)
        d1_r (- r1 r2) d1_c (- c1 c2)
        d2_r (- r2 r1) d2_c (- c2 c1)
        c1 (take-while (partial in-bounds? bounds) 
                       (map (fn[n] [(+ r1 (* n d1_r)) (+ c1 (* n d1_c))])
                            (range)))
        c2 (take-while (partial in-bounds? bounds) 
                       (map (fn[n] [(+ r2 (* n d2_r)) (+ c2 (* n d2_c))])
                            (range)))]
    (concat c1 c2)))
      
(defn solution-2 [s] 
  (let [data (shape-input s)
        nrows (count data)
        ncols (count (data 0))] 
    (as-> s x
          (antenna-positions x)
          (group-by first x)
          (update-vals x (fn [coll] (mapv second coll)))
          (map (fn [kv] (all-pairs (val kv))) x)
          (reduce into x)
          (map (fn [pair] (anti-nodes2 [nrows ncols] (pair 0) (pair 1))) x)
          (reduce concat x)
          (distinct x)
          (count x))))

(comment 
  (solution-2 example-input) ;; 34
  (solution-2 (slurp "resources/input/2024/input_08"))) ;; 1231

(defn s2 []
  (solution-2 (slurp "resources/input/2024/input_08")))

