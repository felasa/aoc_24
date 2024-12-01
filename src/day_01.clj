(ns day-01
  (:require [clojure.string :as s]
            [clojure.set :as set]))


(def example-input 
    (-> 
      "3 4\n4 3\n2 5\n1 3\n3 9\n3 3"
      (s/split #" +|\n")
      ((partial map parse-long))))

(def puzzle-input
  (-> "resources/input/input_01"
      slurp
      (s/split #" +|\n")
      ((partial map parse-long))))
      
(defn left [seq]
  (take-nth 2 seq))

(defn right [seq]
  (take-nth 2 (rest seq)))

(comment 
  (left example-input) ;; (3 4 2 1 3 3)
  (right example-input)) ;; (4 3 5 3 9 3)

(defn solution-1
  [seq]
  (reduce + (map #(abs (- %1 %2)) (sort (left seq)) (sort (right seq)))))

(comment 
  (solution-1 example-input) ;; 11
  (solution-1 puzzle-input)) ;; 1651298

;;PART 2

(update-vals (group-by identity  (left example-input)) count)
(update-vals (group-by identity (right example-input)) count)

(defn solution-2 [coll]
  (let [left (left coll)
        right (right coll)
        left-counts (update-vals (group-by identity left) count)
        right-counts (update-vals (group-by identity right) count)]
    (reduce + (map (fn [[k v]] (* k v (right-counts k 0))) left-counts))))

(comment 
  (solution-2 puzzle-input)) ;; 21306195

(defn s1
  "Invoke me with clojure -X day-01/s1"
  [a]
  (println (solution-1 puzzle-input)))

(defn s2
  "Invoke me with clojure -X day-01/s2"
  [a]
  (println (solution-2 puzzle-input)))

