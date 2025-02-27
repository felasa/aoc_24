(ns day-03.previous
  (:require [clojure.string :as str]))

(defn read-data
  [file] 
  (-> file
       slurp
       str/split-lines))

(def data (read-data "resources/data/input_3"))
(def example-data (read-data "resources/data/example_3_1"))

(comment 
  (count data) ;; 140
  (count (first data))) ;; 140

(def re-digit #"\d+")
(def re-symbols #"[^\d|\.]")
(comment 
  (def symbols (set (flatten (map #(re-seq re-symbols %) data)))))  
;; #{nil "=" "*" "%" "/" "-" "&" "#" "+" "$" "@"}

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
      ;; if theres not an accum digit or doesnt have neighbors reset and go next row
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

(comment
  (def result_0 (walk-data data))
  (def result (reduce + (map #(Integer/parseInt %) result_0)))
  result)

(defn solution-1 []
  (reduce + (map #(parse-long %) (walk-data data))))
;(require '[criterium.core :refer [bench]])
;(bench (solution-1)) ;; 525911
;;; END SOLUTION PART 1

;;; START PART 2
;; imma try getting coords again but walking the data instead of getting clever with regex

(defn get-positions
  [data]
  (loop [row 0 col 0
         digit nil col-start nil
         result []]
    (if (>= row (count data))
      result
      (if (>= col (count (get data row)))
        (recur (inc row) 0
               nil nil ;not detecting edge digits on the right
               (if (nil? digit)
                 result
                 (conj result 
                       [digit [row col-start]
                              [row (+ col-start (dec (count digit)))]])))
        (let [current (data-point [row col] data)]
          (if (= current \.)
            (recur row (inc col)
                   nil nil 
                   (if (nil? digit) 
                     result
                     (conj result 
                           [digit [row col-start] [row (+ col-start (dec (count digit)))]])))
            (if (is-symbol? current)
              (recur row (inc col) nil nil 
                     (if (nil? digit)
                       (conj result [current [row col]])
                       (conj result 
                             [digit [row col-start] [row (+ col-start (dec (count digit)))]]
                             [current [row col]])))
              (recur row (inc col) (str digit current)
                     (if (nil? col-start)
                       col
                       col-start)
                     result)))))))) 
(comment 
  (get-positions data))
(defn between 
  "Return true if lb <= x <= ub"
  [x lb ub]
  (and (<= lb x) (>= ub x)))

(defn find-neighbor-digits
  "Given a coordinate and a list of positions, return the digits
  that are neighbors to that coord"
  [coords positions]
  (let [[i j] coords]
    (->>
      positions
      (remove #(is-symbol? (get % 0)))
      (filter #(between (get (get % 1) 0) (dec i) (inc i)))
      (filter #(and (<= (get (get % 1) 1) (inc j)) (>= (get (get % 2) 1) (dec j)))))))
      
(defn get-result-2
  [positions]
  (->>
    positions
    (filter #(= \* (get % 0)))
    (map #(find-neighbor-digits (get % 1) positions)) 
    (filter #(= (count %) 2))
    (map #(* (Long/parseLong (first (first %))) (Long/parseLong (first (second %)))))
    (reduce + (long 0))))

;;PART 2 RESULT
(get-result-2 (get-positions data))
(comment 
  (spit "3-2-ratios.txt" (str/join "\n" (map str (get-result-2 (get-positions data))))))
;;END PART 2
