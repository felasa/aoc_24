(ns aoc-1
  (:require [clojure.string]))

(def replacement-map {"one"  "1e" 
                      "two"  "2o"
                      "three" "3e"
                      "four" "4r"
                      "five" "5e"
                      "six" "6x"
                      "seven" "7n"
                      "eight" "8t"
                      "nine" "9e"})

(def s-digits ["one" "two" "three"
               "four" "five" "six"
               "seven" "eight" "nine"])

(defn is-number? 
  [character]
  (>= (Character/digit character 10) 0))

(defn not-number?
  [character]
  (not (is-number? character)))

(defn first-and-last-digit
  "Primer y ultimo digito de cadena"
  [string]
  (loop [obj string 
         first-d nil
         last-d nil]
    (if (empty? obj)
      (str first-d last-d)
      (let [head (first obj)
            tail (rest obj)]
        (if (not-number? head)
          (recur tail first-d last-d)
          (if (nil? first-d)
            (recur tail head head)
            (recur tail first-d head)))))))

(defn replace-word-digit [digit string]
    (clojure.string/replace string digit (get replacement-map digit)))

(defn word-to-digit [string]
  (loop [result string]
    (let [positions (map (fn [pattern] (clojure.string/index-of result pattern)) s-digits)
          found (filter (complement nil?) positions)]
      (if (empty? found)
        result
        (let [first-replacement (.indexOf positions (apply min found))]
          (recur (replace-word-digit (get s-digits first-replacement) result)))))))

(defn code-from-string [s]
  (-> s
    (word-to-digit)
    (first-and-last-digit)))

(defn get-result 
  [lines]
  (->> lines 
     (map word-to-digit)
     (map code-from-string)
     (map (fn [x] (Integer/parseInt x)))
     (reduce +)))

(def test-values 
  ["two1nine"   
   "eightwothree" ;eight AND two: 823
   "abcone2threexyz"
   "xtwone3four"
   "4nineeightseven2"
   "zoneight234"
   "7pqrstsixteen"])

(comment 
  (def leido 
    (clojure.string/split-lines
      (slurp "input"))))
;; solucion
(comment
  (get-result leido))
  
(comment 
  (#(Integer/parseInt %)
    (code-from-string 
      (word-to-digit "onetwothreetwo"))))

(comment
  (map word-to-digit test-values)
  (map code-from-string test-values)
  (get-result leido))

(comment 
  (def w2d-leido (map word-to-digit leido))
  (take 10 (map vector leido w2d-leido)))
