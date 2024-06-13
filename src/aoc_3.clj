(ns aoc-3
  (:require [clojure.string :as str]))

(defn read-data
  [file] 
  (-> file
       slurp
       str/split-lines))

(def data (read-data "input_3"))
(def example-data (read-data "example_3_1"))

(comment 
  (count data) ;; 140
  (count (first data))) ;; 140

(def re-digit #"\d+")
(def re-symbols #"[^\d|\.]")
(comment 
  (def symbols (set (flatten (map #(re-seq re-symbols %) data)))))  
;; #{nil "=" "*" "%" "/" "-" "&" "#" "$" "@"}

(defn is-symbol?
  [char]
  ((complement nil?) (re-find re-symbols (str char))))

(defn ne-conj
  "like conj but returns coll if elem is nil" 
  [coll elem]
  (if (nil? elem) coll (conj coll elem)))

(defn data-point 
  "get the char at [i,j] from data"
  [coords data]
  (let [[i j] coords]
    (get (get data i) j)))

(def neighbor-deltas 
    '([-1  1] [0  1] [1  1] 
      [-1 -1] [0 -1] [1 -1]
      [-1  0]        [1  0]))

(defn add-coords 
  [coords_1 coords_2]
  (let [[x_1 y_1] coords_1
        [x_2 y_2] coords_2]
    [(+ x_1 x_2) (+ y_1 y_2)]))

(defn check-neighbors 
  [coords digit lookup-data]
  (if (nil? digit)
    ;ugly, refactor this?
    (some true? 
      (remove nil? 
        (map 
          is-symbol? 
          (map #(data-point % lookup-data) 
               (map #(add-coords coords %) neighbor-deltas)))))
    (some true? 
      (remove nil? 
        (map 
          is-symbol? 
          (map #(data-point % lookup-data) 
               (map #(add-coords coords %) (take 3 neighbor-deltas)))))))) 

(defn walk-data
  "very slow solution to part 1. give it the data and it'll spit the numbers" 
  [data]
  (loop [row 0
         column 0
         digit nil 
         result []
         neighbor-status false]
    ;; if gone over the columns, go next row, add digit to result if it has neighbors
    (if (> column (dec (count (get data row))))
      ;; if theres not an accum digit or doesnt have neightbors reset and go next row
        (if (or (nil? digit) (not neighbor-status))
          (recur (inc row) 0 nil result false)
          ; else add digit to result and go next row
          (recur (inc row) 0 nil (conj result digit) false))
      ;; if gone over rows we're done. add digit to result if needed and return it 
      (if (> row (dec (count data)))
        (if neighbor-status (ne-conj result digit) result)
        ;; in the meantime walking 'till i find digits 
        (let [current (str (get (get data row) column))]
          (if (nil? (re-find #"\d" current))
            ; if no digit found reset accum digit, 
            ;    add possible accum digit to result if it has neighbors, gg go next
            (if neighbor-status
              (recur row (inc column) nil (ne-conj result digit) false)
              (recur row (inc column) nil result false))
            ; if we found a digit append and update neighbor status
            (if neighbor-status
              ; no need to check for neighbors?
              (recur row (inc column) (str digit current) result true)
              ; update neightbor status
              (recur row (inc column) (str digit current) result 
                     (check-neighbors [row column] digit data)))))))))

(def result_0 (walk-data data))
(def result (apply + (map #(Integer/parseInt %) result_0)))
result
;;; END SOLUTION PART 1


;;; STUFF FOR FAILED SOLUTION TO PART 1 KEEPING IT HERE SOME MIGHT BE USEFUL

(defn all-index-of
  "like index-of but gives me all the idx for matches"
  [string substring]
  (loop [from-idx 0
         result []]
    (if (nil? (str/index-of string substring from-idx))
        result
        (let [idx (str/index-of string substring from-idx)]
             (recur (inc idx) (conj result idx)))))) 
  
;FIX: FOR DIGITS PARTIAL MATCHES POSITIONS ARE RETURNED TOO i.e. 34 matches 34 and 2345 
;not the way to go for digits, symbols might be ok still
(defn get-re-match-positions
  "Find position (as index) of symbols in line"
  [line re]
  (as-> 
    line x 
    (re-seq re x)
    (distinct x)
    (map vector x (map #(all-index-of line %) x))))
   ; (flatten x)
   ; (sort x)))
    ;(apply sorted-set x)))

(defn get-re-positions 
  [line re]
  (as->
    line x
    (get-re-match-positions x re)
    (map #(% 1) x)
    (flatten x)
    (sort x)))

(defn arrange-coords
 "Given a row position and vector or digit and column column positions 
  rearange so we get digit and full coordinates [i,j] rows:i columns: j" 
  [idx digit-jpos]
  (let [[digit j-coords] digit-jpos]
    (vector digit (map #(vector idx %) j-coords))))     

(defn parse-line
 "Given a line of digit locations rearrange to get coordinates" 
  [row-positions]
  (let [[idx listado] row-positions]
    (map #(arrange-coords idx %) listado))) 

(defn parse-coords
 "Arrange all the word positions" 
 [w-positions]
 (map parse-line w-positions))

(defn get-symbol-positions
  [line]
  (get-re-positions line #"[^\d|\.]"))

(defn get-number-positions
  [line]
  (get-re-positions line #"\d+"))

(def symbol-postions
  (apply vector (mapv get-symbol-positions data)))

(def number-positions
  (apply vector (map get-number-positions data)))

(defn concata 
  "custom concat forgot what it did. deleted the original"
  [idx coll]
  (map #(vector idx %) coll))
 
(def all-symbol-coords
  (->>
    (map-indexed concata (take 10 symbol-postions)) 
    (apply clojure.core/concat)))

