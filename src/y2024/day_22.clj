(ns day-22 
  (:require
    [clojure.string :as s]))

(def example-input "1
10
100
2024")

(defn parse-input 
  [s]
  (->> s s/split-lines (map parse-long)))

(comment (parse-input example-input))

(defn next-secret 
  [secret]
  (let [A (* secret 64)
        secret (bit-xor A secret)
        secret (mod secret 16777216)
        D (quot secret 32) ;rounding to closest int gives wrong result ???
        secret (bit-xor D secret)
        secret (mod secret 16777216)
        G (* secret 2048)
        secret (bit-xor secret G)
        secret (mod secret 16777216)]
    secret))

(defn solution-1 [s]
  (->> s parse-input 
       (map #(nth (iterate next-secret %) 2000))
       (reduce +)))

(comment 
  (solution-1 example-input) ;; 37327623
  (solution-1 (slurp "resources/input/2024/input_22")))  ;; 18941802053

;; PART 2

(defn ones-digit
  [number]
  (mod number 10))

(defn deltas
  [coll]
  (map - (rest coll) coll))  

(defn patterns-n-values 
  "Returns a map of all the 4 delta sequences and the value at that sequence fro the first 2000
   changes"
  [seed]
  (let [;seed 1
        values (->> seed (iterate next-secret) (map ones-digit))
        deltas (deltas values)
        patterns (partition 4 1 deltas)]
    (reduce merge (reverse (take 1997 (map #(assoc {} (vec %1) %2) patterns (drop 4 values)))))))


(defn best-pattern 
  "Returns the pattern with the highest sum and its value"
  [seeds]
  (->> seeds
       (map patterns-n-values)
       (apply (partial merge-with +))
       (apply max-key val)))

(defn solution-2 
  [s]
  (->> s parse-input 
       (map patterns-n-values)
       (apply (partial merge-with +))
       (apply max-key val)
       val))

(comment 
  (solution-2 "1\n2\n3\n2024") ;; 23
  (solution-2 (slurp "resources/input/2024/input_22"))) ;; 2218

