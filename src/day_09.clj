(ns day-09
 (:require [clojure.string :as s]))

(def example-input "2333133121414131402")

(defn parse-input 
  [s]
  (loop [i 0
         I 0
         file? true
         res []] 
    (if (>= i (count s)) res
      (let [len (Character/digit (get s i) 10)]  
        (if file? 
            (recur (inc i) (+ I len) false
                   (conj res [(quot i 2) len]))
                   ;(assoc-in res [:file] (conj (:file res) [(quot i 2)  len])))
            (recur (inc i) (+ I len) true
                   (conj res [nil len])))))))
                   ;(assoc-in res [:free] (conj (:free res) [nil  len]))))))))

(comment 
  (parse-input "12335")
  (parse-input example-input) 
  (conj ((split-with #(get % 0) (parse-input example-input)) 0) ["FOO"])) 

(defn compact 
  [disk]
  (let [split (split-with #(get % 0) (if (get (last disk) 0) disk (drop-last disk)))]
    (if (empty? (get split 1)) nil
      (let [[_ free-size] (first (get split 1))
            [file file-size] (last (get split 1))
            L (vec (get split 0)) R (vec (drop-last (rest (get split 1))))
            delta (abs (- file-size free-size))]
        (cond 
          (< file-size free-size) (into (conj L [file file-size] [nil delta]) 
                                        R) 
          (= file-size free-size) (into (conj L [file file-size]) 
                                        R)
          (> file-size free-size) (into (conj L [file free-size]) 
                                        (conj R [file delta]))))))) 
;Stack overflow
(defn compact2
  [disk]
  (let [split (split-with #(get % 0) (if (get (last disk) 0) disk (drop-last disk)))]
    (if (empty? (get split 1)) disk
      (let [[_ free-size] (first (get split 1))
            [file file-size] (last (get split 1))
            L (vec (get split 0)) R (vec (drop-last (rest (get split 1))))
            delta (abs (- file-size free-size))
            compacted (cond 
                        (< file-size free-size) (into (conj L [file file-size] [nil delta]) 
                                                      R) 
                        (= file-size free-size) (into (conj L [file file-size]) 
                                                      R)
                        (> file-size free-size) (into (conj L [file free-size]) 
                                                      (conj R [file delta])))]
         (compact2 (if (get (last compacted) 0) compacted (drop-last compacted))))))) 
       
(comment 
  (parse-input example-input)
  (compact (parse-input example-input))
  (compact2 (parse-input example-input))
  (nth (iterate compact (parse-input example-input)) 7)
  (last (take-while identity (iterate compact (parse-input example-input)))))

(defn fold
  [l r]
  [(+ (l 0) (* (r 0) (apply + (range (l 1) (+ (l 1) (r 1))))))
   (+ (l 1) (r 1))])

(comment
  (reduce fold [0 0] 
          (last (take-while identity (iterate compact (parse-input example-input))))))
;SLOW!
(defn solution-1 
  [s]
  (get (->> s 
            parse-input
            ;compact2
            (iterate compact) 
            (take-while identity) 
            last
            (reduce fold)) 0))

(comment 
  (solution-1 example-input) ;; 1928
  (solution-1 (s/replace (slurp "resources/input/input_09") "\n" ""))) ;; 6471961544878
