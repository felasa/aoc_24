(ns day-04
  (:require [clojure.string :as s]))

(def example-input "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX") 

(defn vdata [input]
  (mapv vec (s/split-lines input)))

(def example-data (vdata example-input))

(defn access-data-at 
  "Helper for matrix lookup"
  [data r c]
  (if-let [row (get data r)]
    (get row c)
    nil))

(defn step
  "get the position moing in direction from row r, column c"
  [direction r c]
  (let [[dr dc] direction]
    [(+ r dr) (+ c dc)]))

(def directions
  [[ 0  1] ;r
   [ 1  1] ;dr
   [ 1  0] ;d
   [ 1 -1] ;dl
   [ 0 -1] ;l
   [-1 -1] ;ul
   [-1  0] ;u
   [-1  1]]) ;ur

(defn get-word-positions
  "Returns the position of a word of len n starting at row, col in direction" 
  [len row col direction]
  (take len (iterate #(apply (partial step direction) %) [row col]))) 

(defn get-words-at-pos
  "for all the directions at position r, c make a 4 len string"
  [data r c]
  (let [coords (map (partial get-word-positions 4 r c) directions)]
    (for [lst coords]
      (apply str (map #(apply (partial access-data-at data) %) lst)))))
        
(defn solution-1 [data]
  (let [nrow (count data)
        ncol (count (data 0))]
    (reduce + (for [r (range nrow) c (range ncol)]
                (count (filter #(= "XMAS" %) (get-words-at-pos data r c)))))))

(comment
  (solution-1 example-data) ;; 18
  (solution-1 (-> "resources/input/input_05" slurp vdata))) ;; 2633

(defn s1 []
  (solution-1 (-> "resources/input/input_05" slurp vdata)))

;PART 2
;; only go down-right to avoid duplicates, we'll check for reversed values, and
;; the dual position
(defn make-word-in-direction 
  "Makes the string for a word of size len in direction starting at row r col c"
  [data direction len r c]
  (apply str (map #(apply (partial access-data-at data) %) 
                  (get-word-positions len r c direction))))


(defn x-mas-at-pos?
  "checks if position r, c cointains an x-mas staring in the down-right direction"
  [data r c]
  (let [cc (+ c 2)
        w (make-word-in-direction data [1 1] 3 r c)]
    (if (or (= w "MAS") (= w "SAM"))
      (let [w2 (make-word-in-direction data [1 -1] 3 r cc)]
        (or (= w2 "MAS") (= w2 "SAM")))
      false)))

(defn solution-2 
  [data]
  (let [nrow (count data)
        ncol (count (data 0))]
    (count (for [r (range 0 nrow)
                 c (range 0 ncol)
                 :let [w (x-mas-at-pos? data r c)]
                 :when w]
             w))))

(comment 
  (solution-2 example-data) ;; 9
  (solution-2 (-> "resources/input/input_05" slurp vdata))) ;; 1936

(defn s2 []
  (solution-2 (-> "resources/input/input_05" slurp vdata)))

