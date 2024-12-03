(ns day-04.solution
  (:require
    [clojure.string :as str]
    [clojure.math :as math]
    [clojure.set :as set]))

(def example-input 
 "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn parse-card 
  [line]
  (let [[card winning own] (str/split line #": +| \| +")
        Card (parse-long (get (str/split card #" +") 1))
        winning (set (map parse-long (str/split winning #" +")))
        own (set (map parse-long (str/split own #" +")))]
    (sorted-map Card {:winning winning :own own})))

(defn parse-input 
  [input]
  (reduce #(into %1 (parse-card %2))
          (sorted-map) ;; IMPORTANT for part 2! (may be a workaround..)
          (str/split-lines input)))

(defn n-won 
  [data card-no]
  (let [card (data card-no)]
    (count (set/intersection (:winning card) (:own card))))) 

(comment 
  (n-won (parse-input example-input) 1)) ;; 4

(defn winnings
  [data]
  (->> (keys data)
       (map (partial n-won data))
       (filter pos?)
       (map #(math/pow 2 (dec %)))
       (reduce +)))

(comment 
  (winnings (parse-input example-input))) ;; 13.0

;; result part 1
(defn solution-1
  [s]
  (winnings (parse-input s)))

(defn s1 []
  (solution-1 (slurp "resources/data/input_4")))

(comment 
  (s1)) ;; 19855.0


;; PART 2
(defn increase-counts-by-no
  [multiplier data ids] 
  (reduce (fn [kv card-no]
            (assoc-in kv [card-no :count] 
                      (+ (get (data card-no) :count 1) multiplier))) 
          data
          ids)) 

(defn increase-by-winnings
  [data id]
  (let [card (data id)
        n-won (count (set/intersection (:own card) (:winning card)))
        mult (get card :count 1)]
    ((partial increase-counts-by-no mult) data (range (inc id) (+  (inc id) n-won)))))
  
(defn add-counts
  "Adds all the counts from winnings" 
  [data]
  (reduce increase-by-winnings
          data
          (keys data)))

(defn solution-2
  [s]
  (let [data (add-counts (parse-input s))]
    (reduce + (map #(get % :count 1) (vals data)))))

(defn s2 [] (solution-2 (slurp "resources/data/input_4")))
        
;FINAL RESULT
(comment 
   (s2)) ;; 10378710
