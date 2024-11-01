(ns day-01.discarded
  (:require [clojure.string :as string]))

(defn is-number? 
  "Returns true if char represents a digit, false if not"
  [char]
  (>= (Character/digit char 10) 0))

(def is-not-number?
  (complement is-number?))

;; Removed because it travels the whole string to find the last one
(defn first-and-last-digit
  "Returns the first and last digits of a string, concatenated"
  [string]
  (loop [remaining string 
         first-digit nil
         last-digit nil]
    (if (empty? remaining)
     (str first-digit last-digit)
     (let [head (first remaining)
           tail (rest remaining)]
       (if (is-not-number? head)
           (recur tail first-digit last-digit)
           (if (nil? first-digit)
               (recur tail head head)
               (recur tail first-digit head)))))))

;; STUFF FOR PART2

;; This map replaces a word that translates to a digit to a string that contains
;; that digit plus the last character, that is beacuse in a word like eightwothree
;; *eight* and *two* overlap
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

(defn replace-word-digit
  "Replaces the word for digit with the string in the replacement map"
  [digit string]
  (string/replace string digit (replacement-map digit)))

(defn word-to-digit
  "Does all the replacements in a string for word-digits"
  [string]
  (loop [result string]
    (let [positions (map (fn [pattern] (string/index-of result pattern)) s-digits)
          found (filter (complement nil?) positions)]
      (if (empty? found)
        result
        (let [first-replacement (.indexOf positions (apply min found))]
          (recur (replace-word-digit (get s-digits first-replacement) result)))))))

(defn code-from-string
  "Replaces all word digits to character digits then extractsfirst and last"
  [s]
  (-> s
    (word-to-digit)
    (first-and-last-digit)))

(defn get-result 
  "Applies problem solution to ca collection of strings"
  [lines]
  (->> lines 
     (map word-to-digit)
     (map code-from-string)
     (map (fn [x] (Integer/parseInt x)))
     (reduce +)))

(def leido 
  (clojure.string/split-lines
    (slurp "resources/data/input")))
;; solucion
(get-result leido) ;; 54078

;Another idea, still travels the whole string for the last digit. and it gets it wrong
(def string->digit {"one"   1 
                    "two"   2
                    "three" 3
                    "four"  4
                    "five"  5
                    "six"   6
                    "seven" 7
                    "eight" 8
                    "nine"  9})

(defn partial-d-string 
  "Returns true if s is a substring from a starting position 
   of some element of coll"
  [s coll]
  (some true? (map #(string/includes? % s) (map #(subs % 0 (min (count s) (count %))) coll))))

(defn get-code 
  "Can we travel the string instead of doing all the replacements, then finding the code?"
  ; Fails with 22fourninetzfourfsnxjglthreeeight
  [string]
  (loop [remaining string
         first-digit nil 
         last-digit nil 
         current nil]
      (if (empty? remaining)
        (str first-digit (if last-digit last-digit first-digit))
        (let [head (first remaining)
              tail (rest remaining)]
          (if (is-number? head)
            (if (nil? first-digit)
              (recur tail head last-digit nil)
              (recur tail first-digit head nil))
            (if (string->digit (str current head))
              (if (nil? first-digit)
                (recur tail (string->digit (str current head)) last-digit head)
                (recur tail first-digit (string->digit (str current head)) head))  
              (if (partial-d-string (str current head) s-digits)
                (recur tail first-digit last-digit (str current head))
                (recur tail first-digit last-digit (str head))))))))) 
        
