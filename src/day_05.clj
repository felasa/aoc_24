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

(defn split-with-ex
  "Split with but exludes the matching line"
  [pred coll]
  [(take-while pred coll) (rest (drop-while pred coll))])

(defn parse-rules
  "Takes the rules and returns a key map grouped by 
   the leading number and a collection of following numbers"
  [ruleset]
  (let [g (->> ruleset
              (map #(s/split % #"\|"))
              (map (partial map parse-long))
              ;(map (partial apply hash-map))))
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
      ;if-let?
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
  (solution-1 example-input)   ;; 143
  (solution-1 (slurp "resources/input/input_05"))) ;; 7024

(defn s1 []
  (solution-1 (slurp "resources/input/input_05"))) 
;; PART 2

(let [data (parse-input example-input)
      rules (data :rules)
      update-set (data :updates)]
  (filter (complement (partial validate-updates rules)) update-set))

(defn swap-index
  "Lazy seq from 0 with numebrs i and j swapped"
  [i j]
  (if (< i j)
    (concat (range 0 i) (list j) (range (inc i) j) (list i) (map #(+ % j 1) (range)))
    (concat (range 0 j) (list i) (range (inc j) i) (list j) (map #(+ % j 1) (range)))))

