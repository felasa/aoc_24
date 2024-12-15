(ns day-11
  (:require [clojure.string :as s]))

(def example-input "125 17")

(defn blink
  [n] 
  (cond 
     (= n 0) (seq [1])
     (even? (count (str n))) 
     (map #(parse-long (apply str %)) (split-at (quot (count (str n)) 2) (str n)))
     :else (seq [(* n 2024)])))

(defn flat-blink
  [coll]
  (mapcat blink coll))

(defn parse-input [s]
  (map parse-long (s/split s #" +")))

(defn solution-1 [s]
  (as-> s x
       (parse-input x)
       (iterate flat-blink x)
       (nth x 25)
       (count x)))
       
(comment 
  (solution-1 example-input) ;; 55312
  (solution-1 (s/replace (slurp "resources/input/input_11") "\n" ""))) ;; 183484

;; PART 2
;; Memoization and recursion on the COUNTS not the value itsef
(def count-after-n-blinks
  (memoize
    (fn [times coll] 
      (if (empty? coll) 0
        (condp = times
          0 (count coll) 
          1 (+ (count-after-n-blinks 0 (blink (first coll)))
               (count-after-n-blinks 1 (rest coll)))                       
          (+ (count-after-n-blinks (dec times) (blink (first coll)))
             (count-after-n-blinks times (rest coll))))))))

(defn solution-2 [s]
  (->> s
       (parse-input)
       (count-after-n-blinks 75)))

(comment 
  (solution-2 (s/replace (slurp "resources/input/input_11") "\n" "")) ;; 218817038947400
  ;; better solution-1
  (count-after-n-blinks 25 [8069 87014 98 809367 525 0 9494914 5])) ;; 183484

(defn s2 []
  (solution-2 (s/replace (slurp "resources/input/input_11") "\n" "")))

