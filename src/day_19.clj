(ns day-19
  (:require [clojure.string :as s]))


(def example-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse-input [s]
  (let [sp (-> s s/split-lines)
        patterns (s/split (sp 0) #", ")
        designs (subvec sp 2)]
    {:patterns patterns :designs designs}))

(comment
   (parse-input example-input))
  
(defn sub-starting
  "return a collection of design with patterns removed form the start if applicable"
  [patterns design]
  (let [len (count design)
        re-ps (map (fn [s] (re-pattern (str "^" s))) patterns)
        subbed (map #(s/replace design % "") re-ps)]
    (filter #(< (count %) len) subbed)))

;To acount for orderings we check every tree of subsets until one is empty"
; For part 2 we could continue and count how many empties, but when to stop"
(defn design-possible? 
  "Returns if design is possible to form with patterns"
  [patterns design]
  (loop [to-do (list design)] 
    (if (not-every? #(not= % "") to-do) true
      (if (empty? to-do) false
        (recur (mapcat (partial sub-starting patterns) to-do))))))

; Makes solutionn bearable but probably the solution is not a very good one
(defn remove-redundant-patterns 
  "Removes from patterns all those thaat can be from other patterns"
  [patterns]
  (loop [n-pats 1
         patterns (sort-by count patterns)]
    (if (= n-pats (count patterns)) patterns
      (let [[pats des] (split-at n-pats patterns)]
        (recur (inc n-pats) 
               (concat pats (filter (complement (partial design-possible? pats)) des)))))))

(defn solution-1 
  [s]
  (let [;s example-input
        data (parse-input s)
        patterns (:patterns data)
        minimal-patterns (remove-redundant-patterns patterns)
        designs (:designs data)]
    (->> designs 
         (pmap (partial design-possible? minimal-patterns))
         (filter true?)
         count)))              

(comment 
  (solution-1 example-input) ;; 6
  (solution-1 (slurp "resources/input/input_19"))) ;; 317

