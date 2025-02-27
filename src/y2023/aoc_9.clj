(ns y2023.aoc-9 
  (:require
    [clojure.string :as str]))

(def example-input 
  [[0 3 6 9 12 15]
   [1 3 6 10 15 21]
   [10 13 16 21 30 45]])

(defn parse-input 
  [file]
  (->> 
    file 
    slurp 
    str/split-lines
    (mapv #(str/split % #" "))
    (mapv #(mapv parse-long %))))

(defn s-diff 
  [sq]
  (if (second sq)
    (into [(- (second sq) (first sq))] (s-diff (rest sq))) 
    ;(vector (cons (- (second sq) (first sq)) (s-diff (rest sq))))
    nil))

(def puzzle-input (parse-input "resources/input/2023/input_9"))

(defn diff-till-0 
  [sq]
  (loop [acc [(into [] sq)]
         current sq]
    (if (every? zero? current)
      acc 
      (let [dff (s-diff current)]
        (recur (conj acc dff)
               dff)))))

(defn predict 
  [vctors]
  (if (<= (count vctors) 2)
    (mapv last vctors)
    (let [sub (predict (rest vctors))]
      (into [(+ (last (first vctors)) (first sub))] sub))))
;result part 1
(->> 
  puzzle-input 
  (mapv diff-till-0)
  (mapv predict)
  (map first)
  (apply +))

;; PART 2 

(defn extrapolate 
  [vctors]
  (if (<= (count vctors) 2)
    (mapv first vctors)
    (let [sub (extrapolate (rest vctors))]
      (into [(- (first (first vctors)) (first sub))] sub))))

; result to part 2
(->> 
  puzzle-input 
  (mapv diff-till-0)
  (mapv extrapolate)
  (map first)
  (apply +))

