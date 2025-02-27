(ns y2023.day-05.solution
  (:require [clojure.string :as s]))

(def example-input (slurp "resources/input/2023/example_5_1"))

(defn split-blanks
  "splits input by blank lines to identify map sections"
  [lines]
  (when-let [s (seq lines)]
    (let [[xs tys] (split-with (complement (partial = "")) s)
          ys (rest tys)]
      (if ys
        (lazy-seq (cons xs (split-blanks ys)))
        xs))))

(defn parse-seeds
  "makes a key val for the seeds"
  [string]
  (let [numbers (rest (s/split string #": +| +"))]
    (assoc {} :seeds (map parse-long numbers))))

(defn trasform-encoding
  "Transfors map encoding from [to from len] format to [low high delta] format"
  [to from len]
  [from (+ from len -1) (- to from)])

(defn parse-map 
  "Creates a key val with the name of the map and the map enconding"
  [line]
  (let [map-id (first line)
        codes (rest line)]
    {(keyword (re-find #"[a-z\-]+" map-id))
     (->> 
        codes
        (map #(s/split % #" +"))
        (mapv #(mapv parse-long %))
        (mapv #(apply trasform-encoding %)))}))      

(defn parse-lines
  "Creates a map with all the data, seeds and maps" 
  [lines]
  (let [h (first lines)
        tail (rest lines)]
    (conj  
      (parse-seeds (first h))
      (reduce merge (map parse-map tail)))))

(defn apply-map 
  [n intervals]
  (loop [remaining intervals]
    (if (empty? remaining)
      n
      (let [[low high delta] (first remaining)]
        (if (<= low n high) 
            (+ n delta)
            (recur (rest remaining)))))))

(defn solution-1
  [s]
  (let [data (-> s s/split-lines split-blanks parse-lines)] 
    (reduce min (map #(reduce apply-map % (vals (rest data))) (data :seeds)))))

(solution-1 (slurp "resources/input/2023/input_5")) ;; 457535844

;; PART 2

(def example-data (-> example-input s/split-lines split-blanks parse-lines))
(def puzzle-data (-> "resources/input/2023/input_5" slurp s/split-lines split-blanks parse-lines))


(defn min-for-interval
  "Return the minim soil for values in interval from start (inclusive) 
   to start+len (exclusive)"
  [data start len]
  (reduce min (map #(reduce apply-map % (vals (rest data))) (range start (+ start len)))))

(comment 
  (min-for-interval puzzle-data 515785082 2) ;;        4202284817
  (min-for-interval puzzle-data 515785082 10000000)) ;; 490136624 took a bit

(defn get-interval-seeds
  "Return a seq of pairs representing the seed intervals"
  [data]
  (partition 2 (data :seeds)))

(comment
  (mapv #(apply (partial min-for-interval example-data) %)
       (get-interval-seeds example-data)) ;; [46 56]
  (pmap #(apply (partial min-for-interval puzzle-data) %)
       (get-interval-seeds puzzle-data))) 
;; (429431694
;;  41222968
;;  186829220
;;  475876376
;;  1081323768
;;  2660261620
;;  68608231
;;  2734303452
;;  154835034
;;  2353465194)
(defn solution-2 
  "brute force not recomended but can be done"
  [data]
  (prn "This will take a while")
  (apply min (pmap #(apply (partial min-for-interval data) %)
                  (get-interval-seeds data)))) 
;; there's surely a more efficient and clever solution. 
;; an idea would be to compose the intervals for a final map and just map
;; the starting points (its piece-wise monotonically increasing) or compute 
;; the inverse image of the whole chain
(defn s2 []
  (solution-2 puzzle-data))

(comment
  (s2)) ;; 41222968
