(ns aoc-7 
  (:require
    [clojure.string :as str]
    [clojure.math :as math]))

(defn parse-input 
  [file]
  (->> 
    (slurp file)
    (str/split-lines)
    (map #(str/split % #" "))
    (map #(vector (get % 0) (parse-long (get % 1)))))) 

(def cards (seq "AKQJT98765432"))

(def card-ranks 
  (zipmap cards (range 12 -1 -1)))

(defn position-score 
  [hand]
  (reduce + 
          (map-indexed #(* (math/pow 13 (- 4 %1)) 
                           (get card-ranks %2)) 
                       hand)))

(defn get-hand-type
  [hand]
  (let [char-hand (seq hand)
        grouped (group-by identity char-hand)]
    (into [] 
          (sort-by #(- (key %))
                   (update-vals (group-by second (update-vals grouped count))
                                count)))))

;; 400,000 is higher than the maximum position score
(def type-score {[[5 1]]       28E6
                 [[4 1] [1 1]] 24E6
                 [[3 1] [2 1]] 20E6
                 [[3 1] [1 2]] 16E6
                 [[2 2] [1 1]] 12E6
                 [[2 1] [1 3]]  8E6
                 [[1 5]]        4E6}) 

(defn hand-score [hand]
  (+ (get type-score (get-hand-type hand))
     (position-score hand)))

(defn winnings 
  [input]
  (reduce + 
          (map-indexed #(* (inc %1) (get %2 1)) 
                       (sort-by #(nth % 2) 
                                (map 
                                   #(conj % (hand-score (get % 0)))
                                   input)))))
;RESLUT TO PART 1
(winnings (parse-input "input_7"))

