(ns y2023.day-19
  (:require [clojure.string :as s]))

(def example-input "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")


(defn breadown-workflow
  "Returns a map key being the name of the workflow value
   being the workflow rules"
  [wf]
  (let [v (re-matches 
            #"([a-z]+)\{((.*:.*,)+([a-zA-z]+))\}"
            wf)]
    {(v 1)
     (v 2)}))

(defn parse-rating
  "returns a map with key being the name of rating value its value"
  [s]
  (update-vals (->> (re-seq #"[xmas]=\d+" s)
                    (map #(s/split % #"="))
                    (into {}))
               parse-long))
(comment 
  (breadown-workflow "ex{x>10:one,m<20:two,a>30:R,A}") ;; {"ex" "x>10:one,m<20:two,a>30:R,A"}
  (parse-rating "{x=2127,m=1623,a=2188,s=1013}")) ;; {"x" 2127, "m" 1623, "a" 2188, "s" 1013}

(defn build-rule
  "given a rule in string format place its components in a vector"
  [s]
  (let [re #"([a-z]+)([<>])(\d+):([a-z]+|[AR])|([a-z]+|[AR])" 
        parts (->> (re-matches re s) rest (remove nil?)
                   (into []))]
    parts))

(defn build-set
  "Applies build-rule to a worflow rule-set"
  [s]
  (->> (s/split s #",")
       (map build-rule)))

(defn parse-input [s]
  (let [[workflows ratings]
        (->> (s/split-lines s)
             (split-with #(not= "" %)))]
    {:workflows (update-vals (into {} (map breadown-workflow workflows))
                             build-set)
     :ratings (mapv parse-rating (drop 1 ratings))}))

(comment 
  (build-rule "a<2006:qkq") ;; ["a" "<" "2006" "qkq"]
  (build-rule "A")) ;; ["A"]
  
(defn apply-rule
  "applies a single rule to ratings. returns either the next workflow
  or accepted rejected as bool"
  [rule ratings]
  (let [[who what than then ] rule]
    (if what
      (if (eval (list (symbol what) (ratings who) (parse-long than)))
        (case then
          "A" true
          "R" false
          then)
        nil)
      (case who
        "A" true
        "R" false
        who)))) 

(comment 
  (apply-rule ["a" "<" "2006" "qkq"] {"x" 1 "m" 1 "a" 2005 "s" 1}) ;; "qkq"
  (apply-rule ["zqqs"] {"x" 1 "m" 1 "a" 2005 "s" 1}) ;; "zqqs"
  (apply-rule ["rfg"] {"x" 787, "m" 2655, "a" 1222, "s" 2876})) ;; "rfg"
  
(defn accepted?
  "Given the set of workflows and a rating return if accepted as true"
  [workflows ratings]
  (loop [rem (workflows "in")]
    (if (seq? rem)
      (let [applied (apply-rule (first rem) ratings)]
        (cond (nil? applied) (recur (rest rem))
              (boolean? applied) applied
              :else (recur (workflows applied))))
      false)))
     
(defn solution-1 [s]
  (let [{workflows :workflows ratings :ratings} (parse-input s)]
    (->> ratings 
         (filter (partial accepted? workflows))
         (map #(reduce + (vals %)))
         (reduce +))))

(comment 
  (solution-1 example-input) ;; 19114
  (solution-1 (slurp "resources/input/2023/input_19"))) ;; 395382

