(ns y2023.aoc-7 
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
  (reduce
    + 
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
;RESULT TO PART 1
(winnings (parse-input "resources/input/2023/input_7"))
;; PART 2
(defn apply-joker 
  [m]
  (if (= (get m \J) 5) 
    m
    (let [max-k (key (apply max-key val (dissoc m \J)))
          j-count (get m \J)]
      (if (nil? j-count)
        m 
        (update (dissoc m \J) max-k #(+ j-count %))))))
      
  
(defn get-hand-type-w-joker
  [hand]
  (let [char-hand (seq hand)
        grouped (group-by identity char-hand)]
    (into [] 
          (sort-by #(- (key %))
                   (update-vals (group-by second (apply-joker (update-vals grouped count)))
                                count)))))

(def card-ranks-w-joker
  (update (zipmap cards (range 13 0 -1)) \J (constantly 0)))

(defn position-score-w-joker
  [hand]
  (reduce
    + 
    (map-indexed #(* (math/pow 14 (- 4 %1)) 
                     (get card-ranks-w-joker %2)) 
                 hand)))

(defn hand-score-w-joker [hand]
  (+ (get type-score (get-hand-type-w-joker hand))
     (position-score-w-joker hand)))

(defn winnings-w-joker 
  [input]
  (reduce + 
          (map-indexed #(* (inc %1) (get %2 1)) 
                       (sort-by #(nth % 2) 
                                (map 
                                   #(conj % (hand-score-w-joker (get % 0)))
                                   input)))))
;; RESULT TO PART 2.
;; could be refactored to avoid rewriting the same functions
(winnings-w-joker (parse-input "resources/input/2023/input_7"))

