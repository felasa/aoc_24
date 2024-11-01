(ns day-01.solution
  (:require [clojure.string :as string]))

(def s-digits ["one" "two" "three"
               "four" "five" "six"
               "seven" "eight" "nine"])

(def string->digit {"one"   1 
                    "two"   2
                    "three" 3
                    "four"  4
                    "five"  5
                    "six"   6
                    "seven" 7
                    "eight" 8
                    "nine"  9})
(defn is-number? 
  "Returns true if char represents a digit, false if not"
  [char]
  (>= (Character/digit char 10) 0))

;May be better than previous attempts because it stops when a digit is found
(defn first-digit-character
  "Return the first character digit in a string s, if any"
  [s]
  (loop [remaining s]
    (if (empty? remaining)
      nil
      (let [head (first remaining)
            tail (rest remaining)]
        (if (is-number? head)
          (str head)
          (recur tail))))))

(defn last-digit-character
  "Return the last character digit in string s, if any"
  [s]
  (first-digit-character (string/join (reverse s))))

(defn first-and-last-digit-character
  [s]
  (str (first-digit-character s) (last-digit-character s)))

(defn solution-1 [] ;; 54601
  (->> "resources/data/input"
       slurp
       string/split-lines
       (map first-and-last-digit-character)
       (map parse-long)
       (reduce +))) 

(defn partial-d-string 
  "Returns true if s is a substring from a starting position 
   of some element of coll"
  [s coll]
  (some true? (map #(string/includes? % s) (map #(subs % 0 (min (count s) (count %))) coll))))

;ditto wrt returning early
(defn find-first-digit
  "Gets first word or character digit, if any"
  [s]
  (loop [remaining s
         acum nil]
    (if (empty? remaining)
      nil
      (let [head (first remaining)
            tail (rest remaining)
            word (str acum head)]
        (if (is-number? head)
          (str head)
          (if (partial-d-string word s-digits)
            (if (string->digit word)
              (str (string->digit word))
              (recur tail word))
            (recur tail (subs word 1 (count word)))))))))
      
(defn find-last-digit 
  "Gets last word or character digit, if any"
  [s]
  (loop [remaining (apply str (reverse s))
         acum nil]
    (if (empty? remaining)
      nil
      (let [head (first remaining)
            tail (rest remaining)
            word (str  acum head)]
        (if (is-number? head)
          (str head)
          (if (partial-d-string word (map string/join (map reverse s-digits)))
            (if (string->digit (string/join (reverse word))) 
              (str (string->digit (string/join (reverse word))))
              (recur tail word))
            (recur tail (subs word 1 (count word)))))))))
      
(defn first-and-last-digit
  "concatenates first an last digits"
  [s]
  (str (find-first-digit s) (find-last-digit s)))
 
(defn solution-2 [] ;; 54078
  (->> "resources/data/input"
       slurp
       string/split-lines
       (map first-and-last-digit)
       (map parse-long)
       (reduce +))) 

(comment 
  (solution-1)
  (solution-2))
