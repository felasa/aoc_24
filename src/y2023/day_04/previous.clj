(ns day-04.previous 
  (:require
    [clojure.string :as str]
    [clojure.math :as math]
    [clojure.set :as set]))

(defn read-data 
 [file]
 (-> file
     slurp
     str/split-lines))

(def example-input (read-data "resources/data/example_4_1"))
(def puzzle-input (read-data "resources/data/input_4"))

(defn parse-card 
  [line]
  (let [[card winning own] (str/split line #": +| \| +")
        Card (parse-long (get (str/split card #" +") 1))
        winning (str/split winning #" +")
        own (str/split own #" +")]
    (hash-map :Card Card :winning winning :own own)))
;    (-> {
;              (assoc :Card (parse-long (get (str/split card #" +") 1)))
;              (assoc :winning (str/split winning #" +"))
;              (assoc :own (str/split own #" +")))))

(comment
  (parse-card (first example-input)))
;; {:Card "1",
;;  :winning ["41" "48" "83" "86" "17"],
;;  :own ["83" "86" "6" "31" "17" "9" "48" "53"]}

(defn parse-input 
  [input]
  (map parse-card input))

(def example-data (parse-input example-input))

(defn in? 
  "true if coll contains elm"
  [elm coll]  
  (some #(= elm %) coll))

(defn n-won 
  [card]
  (as->
    card x
    (map #(in? % (:winning x)) (distinct (:own x))) ;input data has duplicates
    (filter true? x)
    (count x)))

(comment 
  (n-won (first example-data))) ;; 4

(comment
  (map #(assoc % :n-won (n-won %)) example-data)
  (map #(math/pow 2 (dec (:n-won %))) 
       (filter #(> (:n-won % ) 0) (map #(assoc % :n-won (n-won %)) example-data))))

(defn winnings 
  [input]
  (->> input
       (map #(assoc % :n-won (n-won %)))
       (filter #(> (:n-won %) 0))
       (map #(math/pow 2 (dec (:n-won %))))
       (reduce +)))

(winnings example-data) ;; 13.0
(def puzzle-data (parse-input puzzle-input))
;; result part 1
(defn solution-1 [] (winnings puzzle-data))
(comment 
  (solution-1)) ;; 19855.0
;; PART 2
(defn add-winnings
  [input]
  (->> 
    input 
    (map #(assoc % :n-won (n-won %)))
    (map #(assoc % :count 1))))     

(defn between 
  "Return true if lb <= x <= ub"
  [x lb ub]
  (and (<= lb x) (>= ub x)))

(defn add-counts 
  [win-data]
  (loop [card 1
         result win-data]
    (if (> card (count win-data))
      result
      (let [current (first (filter #(= (:Card %) card) result))
            card_no (:Card current)
            won (:n-won current)
            c-count (:count current)
            top (filter #(<= (:Card %) card) result)
            middle (filter #(between (:Card %) (inc card) (+ card won)) result)
            bottom (filter #(> (:Card %) (+ card won)) result)]
        (recur (inc card_no) 
               (concat top 
                       (map #(update % :count + c-count) middle)
                       bottom))))))

(defn result-pt2
  [input-data]
  (->> 
    input-data
    parse-input
    add-winnings
    add-counts
    (map :count)
    (reduce +)))      


(result-pt2 example-input) ;; 30
;FINAL RESLT
(result-pt2 puzzle-input)
