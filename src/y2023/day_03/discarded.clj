(ns day-03.discarded)
;;; STUFF FOR FAILED SOLUTION TO PART 1 KEEPING IT HERE SOME MIGHT BE USEFUL
;;; I should re-try using the approach for part 2 since it was the original idea for 
;;  this to use the coordinates to find the nieghbors, and it's probably faster
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

