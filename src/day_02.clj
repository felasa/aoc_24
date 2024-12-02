(ns day-02
  (:require [clojure.string :as str]))

(def example-input
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn parse-input [s] 
  (->> (str/split-lines s)
       (map #(str/split %  #" +"))
       (map #(map parse-long %))))

(defn diff
  [sq]
  (map - sq (rest sq)))

(defn safe?
  [seq]
  (let [diffs (map - seq (rest seq))]
    (and (or (every? pos? diffs) (every? neg? diffs))
         (every? #(<= (abs %) 3) diffs))))

(defn solution-1 
  [s] 
  (->> (parse-input s)
       (filter safe?)
       count)) 

(comment
  (solution-1 example-input) ;; 2
  (solution-1 (slurp "resources/input/input_02"))) ;; 534
              
(defn s1 [args]
  (println (solution-1 (slurp "resources/input/input_02"))))

;; PART 2
(defn damp-safe?
  "BRUTE FORCING THIS GRR!"
  [sq]
  (if (safe? sq)
    true
    (loop [exclude 0
           ret false]
      (if (>= exclude (count sq))
        ret
        (if (safe? (concat (take exclude sq) (drop (inc exclude) sq)))
          true
          (recur (inc exclude) false))))))

(defn solution-2
  [s]
  (->> (parse-input s)
       (filter damp-safe?)
       count))

(comment 
  (solution-2 example-input) ;; 4
  (solution-2 (slurp "resources/input/input_02"))) ;; 577

(defn s2 [args]
  (println (solution-2 (slurp "resources/input/input_02"))))

