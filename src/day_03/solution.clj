(ns day-03.solution
  (:require [day-01.solution :refer [is-number?]]
            [clojure.string :as string]))

(def input-path "resources/data/input_3")

(defn read-data
  "Reads the puzzle input into an array of string"
  [file]
  (-> file
      slurp
      string/split-lines))

(def data (read-data input-path))

(def example-data
  ["467..114.."
   "...*......"
   "..35..633."
   "......#..."
   "617*......"
   ".....+.58."
   "..592....."
   "......755."
   "...$.*...."
   ".664.598.."])

(def symbols #{\= \* \% \/ \- \& \# \$ \@ \+})

(defn get-whole-number-at
  "If string s at position idx contains a digit return the whole number it is a part of"
  [idx s]
  (if ((complement is-number?) (get s idx))
    nil
    (loop [current-pos (dec idx) ;since we started at a digit we move back
           result (str (get s idx))
           backwards? true]
      (let [current-char (get s current-pos)]
        (if backwards?
          (if (or (< current-pos 0) (not (is-number? current-char)))
            (recur (inc idx) result false)
            (recur (dec current-pos) (str current-char result) true))
          (if (or (>= current-pos (count s)) (not (is-number? current-char)))
            (parse-long result)
            (recur (inc current-pos) (str result current-char) false)))))))

;; much faster than a custom finder
(defn find-re-and-index
  "Given regex re and string s find first match and returns a vector of 
   the match and the position where it starts"
  [^java.util.regex.Pattern re ^String s]
  (let [matcher (re-matcher re s)]
    (if (.find matcher)
      [(.group matcher) (.start matcher)]
      nil)))

(defn neighbor-positions
  "Given a position an length return the positions of neighbors"
  [pos len]
  (let [[row col] pos
        top [[(dec row) (dec col)] (+ len 2)]
        middle-left [[row (dec col)] 1]
        middle-right [[row (+ col len)] 1]
        bottom [[(inc row) (dec col)] (+ len 2)]]
    [top middle-left middle-right bottom]))

(defn correct-interval
  "Fixes an interval given the width and height of the area so it
   doesn't go out of bounds"
  [interval width height]
  (let [[[row col] len] interval]
    (if (or (< row 0) (>= row height) (>= col width))
      nil
      (if (< col 0)
        [[row 0] (- len (- 0 col))]
        (if (>= (+ col len) width)
          [[row col] (- width col)]
          interval)))))

(defn get-chars-at-interval
  "Returns a vector of the characters found in an interval"
  [[[row col] len] data]
  (if (< row 0)
    nil
    (vec (subs (data row) (max col 0) (min (+ col len) (dec (count (data 0))))))))

(defn get-all-chars
  "Returns a set of the distinct chars found"
  [coll data]
  (set (flatten (map #(get-chars-at-interval % data) coll))))

(defn all-matches-and-index
  "Given a regular expresion re finds all the matches in string s with 
   the position where it starts"
  [re s]
  (let [matcher (re-matcher re s)]
    ((fn step []
       (when (.find matcher)
         (cons [(.group matcher) (.start matcher)] (lazy-seq (step))))))))

(defn all-digits-and-index
  "Returns all digit words with their position in s"
  [s]
  (all-matches-and-index #"\d+" s))

(defn all-digits-at-row
  [row data]
  (let [matches (all-digits-and-index (data row))]
    (map (fn [m] 
           (let [[digit col] m]
             [digit [row col]])) 
         matches)))

(all-digits-at-row 0 data)

(defn digit-pos->digit-interval
  "Given a digit position pair and a row index returns the interval 
   the digit occupies"
  [digit-pos row]
  (let [[digit pos] digit-pos]
    [digit [[row pos] (count digit)]]))

(defn find-part-numbers
  "Return all the part numbers in a row"
  [row data]
  (let [height (count data)
        width (count (data 0))]
    (->> row
         (data)
         (all-digits-and-index)
         (map #(digit-pos->digit-interval  % row))
         (map #(vector (% 0) (apply neighbor-positions (% 1))))
         (map #(vector (% 0) (remove nil? 
                                     (map (fn [int] (correct-interval int width height))
                                          (% 1)))))
         (map #(vector (% 0) (get-all-chars (% 1) data)))
         (filter (fn [[d st]] (some #(contains? symbols %) st)))
         (map #(parse-long (% 0))))))

(defn solution-1 []
  (reduce + (mapcat #(find-part-numbers % data) (range (count data)))))
;; PART 2

(defn gear-coords-at-row
  "Given a row and the data returns matches of '*' and start position"
  [row data]
  (let [s (data row)  
        matcher (re-matcher #"\*" s)] 
    ((fn step []
       (when (.find matcher)
         (cons [(.group matcher) [row (.start matcher)]] (lazy-seq (step))))))))

(defn digit-intervals-at-row
  "Given a row and data returns all the digit matches and the interval that cointains it"
  [row data]
  (let [s (data row)
        matcher (re-matcher #"\d+" s)]
    ((fn step []
       (when (.find matcher)
         (let [d (.group matcher)]
           (cons [d [[row (.start matcher)] (count d)]] (lazy-seq (step)))))))))

(defn interval-neighbor?
  "Given coordinate coord and an interval returns if the interval is a neighbor of the
   coordinate"
  [coord interval]
  (let [[c-row c-col] coord
        [[i-row i-col] len] interval
        i-col-end (dec (+ i-col len))]
    (if (or (< i-row (dec c-row))
            (> i-row (inc c-row))
            (> i-col (inc c-col))
            (< i-col-end (dec c-col)))
      false
      true)))

(defn digit-match-neighbor?
  "Given a coord and a digit mathc that cointains the interval returns if the interval
   is a neighbor of the coordinate"
  [coord digit-match]
  (let [[_ interval] digit-match]
    (interval-neighbor? coord interval)))

(defn digit-neighbors
  "Given a coordinate and the data returns all the digit matches that are neighbors of the 
   coordinate"
  [[row col] data]
  (for [rw (range (max 0 (dec row)) (inc (min (inc row) (dec (count data)))))
           dints (digit-intervals-at-row rw data)
           :when (digit-match-neighbor? [row col] dints)]
     dints))

(defn ratio
  "'Ratio' (product) of a collection of string digits"
  [dseq]
  (->> dseq
      (map parse-long)
      (reduce *)))

(defn row-sum-ratios
  "For a row and data computes the sum of 'gear ratios'"
  [row data]
  (let [gear-coords (map #(% 1) (gear-coords-at-row row data))
        neighbor-digit-intervals (map #(digit-neighbors % data) gear-coords)
        neighbor-digits (map #(map (fn [elm] (get elm 0)) %) neighbor-digit-intervals)
        filtered (filter #(= (count %) 2) neighbor-digits)
        ratio-sum (map ratio filtered)]
    (reduce + ratio-sum)))

(defn solution-2 []
  (reduce + (map #(row-sum-ratios % data) (range 0 140))))

(comment 
  (solution-2))
