(ns day-05
  (:require [clojure.string :as s]))

(def example-input "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
")

(defn parse-rules
  "Takes the rules and returns a key map grouped by 
   the leading number and a collection of following numbers"
  [ruleset]
  (let [g (->> ruleset
              (map #(s/split % #"\|"))
              (map (partial map parse-long))
              (group-by first))]
    (update-vals g #(set (map second %)))))
  
(defn parse-updates
  "Makes seqs of the update list"
  [updateset]
  (->> updateset
       (map #(s/split % #","))
       (map (partial map parse-long))))

(defn parse-input
  "Combines parse-rules and parse updates into a single key map"
  [s]
  (let [pred (partial not=  "")
        lines (s/split-lines s)
        [rules updates] [(take-while pred lines) (rest (drop-while pred lines))]]
     {:rules (parse-rules rules)
      :updates (parse-updates updates)}))
     
(defn validate-updates
  "Checks if the seq of updates is consistent with given rules"
  [rules updates]
  (loop [checked (list (first updates))
         remaining (rest updates)]
    (if (empty? remaining) true
      (let [head (first remaining)
            tail (rest remaining)]
        (if (rules head)
          (if (some (rules head) checked)
            false
            (recur (conj checked head) tail)) 
          (recur (conj checked head) tail))))))

(defn get-middle
  "just gets the middle entry f a seq"
  [v]
  ((vec v) (quot (count v) 2)))

(defn solution-1
  [s]
  (let [data (parse-input s)
        rules (data :rules)
        update-set (data :updates)]
    (->> update-set
         (filter (partial validate-updates rules))
         (map get-middle)
         (reduce +))))

(comment 
  (def example-data (parse-input example-input))
  (solution-1 example-input)   ;; 143
  (solution-1 (slurp "resources/input/2024/input_05"))) ;; 7024

(defn s1 []
  (solution-1 (slurp "resources/input/2024/input_05"))) 

;; PART 2

;; Lazy solution using comparators
(defn ordering 
  [rules]
  (fn [l r] (if (and (rules l) ((rules l) r)) true false)))

(comment 
  (let [example-ordering (ordering (:rules example-data))]
    (sort (comparator example-ordering) [75 97 47 61 53]) ;; (97 75 47 61 53)
    (sort (comparator example-ordering) [61 13 29]) ;; (61 29 13)
    (sort (comparator example-ordering) [97 13 75 29 47]))) ;; (97 75 47 29 13)

(defn solution-2 [s]
  (let [data (parse-input s)
        rules (data :rules)
        update-set (data :updates)
        rules-comparator (ordering rules)]
    (->> update-set
         (filter (complement (partial validate-updates rules)))
         (map (partial sort rules-comparator))
         (map get-middle)
         (reduce +))))

(comment 
  (solution-2 example-input) ;; 123
  (solution-2 (slurp "resources/input/2024/input_05"))) ;; 4151

(defn s2 []
  (solution-2 (slurp "resources/input/2024/input_05"))) ;; 4151

