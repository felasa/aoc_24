(ns y2023.day-08
  (:require [clojure.string :as s]))

(def example-input "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

(defn build-graph
  [nodes]
  (loop [to-do nodes
         return {}]
    (if (empty? to-do) return
      (let [[n l r] (s/split (first to-do) #"[^A-Z0-9]+")]
        (recur (rest to-do) (assoc return n {\L l \R r}))))))

(defn parse-input 
  [s]
  (let [;s example-input
        lines (s/split-lines s)
        instructions (lines 0)
        nodes (subvec lines 2)]
    {:starting (seq instructions) :graph (build-graph nodes)}))

(defn travel-till 
  [graph pred directions start]
  (loop [current start
         travel-list directions
         counter 1]
    (if (empty? travel-list)
      (recur current directions counter)
      (let [newpos (get-in graph [current (first travel-list)])]
        (if (or (pred newpos) (> counter 100000)) counter
          (recur newpos (rest travel-list) (inc counter)))))))

(defn solution-1 
  [s]
  (let [lines (s/split-lines s)
        instructions (lines 0)
        nodes (subvec lines 2)
        graph (build-graph nodes)]
    (travel-till graph #(= % "ZZZ") instructions "AAA"))) 

(comment
  (solution-1 example-input) ;; 2
  (solution-1 (slurp "resources/input/2023/input_8"))) ;; 19631

;; PART 2
(def example-input-2 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

;; not used
(defn traverse-checking
  "Traverse nodes infinitely according to instructions appplying fn"
  [graph initial-node instructions fn]
  (let [cycled (cycle instructions)
        instruction (first cycled)
        next-node (get-in graph [initial-node instruction])]
    (lazy-seq (cons (fn initial-node) 
                    (traverse-checking graph next-node (rest cycled) fn)))))  
(defn gcd 
  [a b]
  (loop [a (abs a) b (abs b)]
    (if (zero? b) a 
      (recur b (rem a b)))))

(defn lcm 
  [a b]
  (if (or (zero? a) (zero? b)) 0
    (abs (* b (quot a (gcd a b)))))) 

; tied lazyly cheching each iter but stack kept blowing up 
; is there a stack safe version of traverse-checking?
; solution from https://github.com/Maravedis/advent_code/blob/master/src/advent_of_code/2023/08.clj
;; lcm works because every 'Z' node has the same (possiblly inverted) destinations as
;;  the 'A' nodes, so no matter the direction every cycle is one of the few possible
(defn solution-2 [s]
  (let [;s (slurp "resources/input/2023/input_8")
        data (parse-input s)
        instructions (:starting data)
        graph (:graph data)
        pred #(s/ends-with? % "Z")] 
    (->> graph keys
         (filter #(s/ends-with? % "A"))
         (map (partial travel-till graph pred instructions))
         (reduce lcm))))

(comment 
  (solution-2 example-input-2) ;; 6
  (solution-2 (slurp "resources/input/2023/input_8")) ;; 21003205388413
  (let [s (slurp "resources/input/2023/input_8")
        data (parse-input s)
        graph (:graph data)]
    [(filter #(s/ends-with? (key %) "A" ) graph) 
     (filter #(s/ends-with? (key %) "Z" ) graph)])) 
;; [(["DQA" {\L "MVV", \R "LQJ"}]
;;   ["RGA" {\L "PFP", \R "DQD"}]
;;   ["MJA" {\L "FXB", \R "QVX"}]
;;   ["JMA" {\L "BJB", \R "TJS"}]
;;   ["AAA" {\L "HLR", \R "RKL"}]
;;   ["XHA" {\L "KJK", \R "NMR"}])
;;  (["DHZ" {\L "LQJ", \R "MVV"}]
;;   ["PKZ" {\L "NMR", \R "KJK"}]
;;   ["HPZ" {\L "DQD", \R "PFP"}]
;;   ["ZZZ" {\L "RKL", \R "HLR"}]
;;   ["HJZ" {\L "QVX", \R "FXB"}]
;;   ["FKZ" {\L "TJS", \R "BJB"}])]
