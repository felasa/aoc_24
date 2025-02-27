(ns day-21
  (:require [clojure.string :as s]))

;; This one took me a while. The general idea was to construct mapps from buttons
;;   to neighbors and directions, and sequentially apply them to construct the 
;;   directions needed to achieve button presses. These mappings had to be THE 
;;   shortest sequence of instructions for a given pair of button presses.
;; Turned out this wasn't enough as being short doesn't guarantee that going one
;;  level deeper the instructions are also the shortest, more on that later.


;; At first I did two sepparate neighbor graphs for the numbers keypad and 
;;   the arrow keypad. But for part two for and it's recursive nature it was needed
;;   to be only one.

;; These graphs map neighbors in the button pads and what direction is needed to go 
;;  for one to the other.
(def numpad-graph
  {\0 {\A \> \2 \^}
   \A {\0 \< \3 \^}
   \1 {\2 \> \4 \^}
   \2 {\1 \< \0 \v \3 \> \5 \^}
   \3 {\2 \< \A \v \6 \^}
   \4 {\1 \v \5 \> \7 \^}
   \5 {\4 \< \2 \v \6 \> \8 \^}
   \6 {\5 \< \3 \v \9 \^}
   \7 {\4 \v \8 \>}
   \8 {\9 \> \5 \v \7 \<}
   \9 {\8 \< \6 \v}})

(def arrow-graph
  {\< {\v \>}
   \v {\< \<, \> \>, \^ \^}
   \> {\v \<, \A \^}
   \^ {\v \v, \A \>}
   \A {\^ \<, \> \v}})

;; A unified mapping
(def transition-graph
  (merge-with merge numpad-graph arrow-graph))

;; These few functions help with making a partition function that 
;; includes the matching element and the end of the collection
(defn take-to
  "Take until condition is met including the match"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (not (pred (first s)))
       (cons (first s) (take-to pred (rest s)))
       (cons (first s) nil)))))

(defn drop-at
  "Drops until condition is met excñuding the elem where it happens"
  [pred coll]
  (let [step (fn [pred coll]
               (if-let [s (seq coll)]
                 (if (and s (pred (first s)))
                   (rest s)
                   (drop-at pred (rest s)))
                 coll))]
    (lazy-seq (step pred coll))))

;bugged 
(defn partition-with
  [pred coll]
  (loop [left  (take-to pred coll)
         right (drop-at pred coll)
         ret []]
    (if (seq right)
      (recur (take-to pred right) (drop-at pred right) (conj ret left))
      (conj ret left))))

; this stumped me for a while; how to get ALL possible paths? 
; had to look it up, it's a variation of this 
; https://stackoverflow.com/questions/30002104/traversing-a-graph-in-clojure
(defn paths
  "Returns all the paths in graph from-to"
  [graph from to]
  ;(println {:v visited :f from :t to})
  (partition-with
    #(= % to)
    ((fn step [so-far visited]
       (let [curr (peek so-far)
             neighbors (remove visited (keys (graph curr)))]
         (if (= to curr) so-far
             (mapcat #(step (conj so-far %) (conj visited %)) neighbors))))
     (vector from) #{from})))

(comment 
  (paths transition-graph \1 \7)
  (paths numpad-graph \1 \7))

;; Getting the sortest sequence of directions to go from one button to 
;;  another is not enough since second order effects influence the final count. 

;; consecutive button presses are better because we dont have to move
(defn n-consecutive
  [coll]
  (loop [s (seq coll)
         ret 0]
    (if (empty? s) ret
        (if (= (first s) (second s))
          (recur (rest s) (inc ret))
          (recur (rest s) ret)))))

;; being close to the A button at the end is apparently better (still unsure why)
(def ins-to-digit {\> 2 \^ 2 \v 1 \< 0})

;; so we weight possible directions by the following factors:
;; first factor weight is how many moves.
;; second factor weight: how many repeats. since pressing the same button repeatedly
;;   means higher order dont have to move at all
;; third? end close to \A?
(defn instruction-score
  "For ordering a score that takes into account consecutive repeats"
  [instructions]
  (let [component-1 (count instructions)
        component-2 (n-consecutive instructions)
        component-3 (->> instructions 
                         (map-indexed (fn [idx v] (* 10 idx (ins-to-digit v))))
                         (reduce +))]             
    (+ (* component-1 100000) (* component-2 -10000) (* component-3 -100))))

(defn gen-instrucions
  "Get the moves needed to travel the path"
  [graph path]
  (let [pairs (partition 2 1 path)]
    (map (fn [pair] ((graph (first pair)) (second pair)))  pairs)))

(defn shortest-path
  "Return a shortest path from-to"
  [graph from to]
  (apply min-key (comp instruction-score
                       (partial gen-instrucions graph))
         (paths graph from to)))

(defn pth
  "With the mapping graph generate the best sequence of directions to go from-to"
  [graph from to]
  (gen-instrucions graph
                   (shortest-path graph from to)))

; so for a given code, get the moves interleaving an \A between them
; compose the moves with d-keys paths twice 
(defn complexity
  [code]
  (let [;code "179A"
        wstartpos     (str "A" code)
        ft-code       (partition 2 1 wstartpos)
        path_l1       (map #(apply (partial pth transition-graph) %) ft-code)
        directions    (interleave path_l1 (repeat (list \A)))
        actioned      (flatten directions)
        ft-code-l2    (partition 2 1 (cons \A actioned))
        directions_l2 (interleave (map #(apply (partial pth transition-graph) %)
                                       ft-code-l2) 
                                  (repeat (list \A)))
        actioned_l2   (flatten directions_l2)
        ft-code-l3    (partition 2 1 (cons \A actioned_l2))
        directions_l3 (interleave (map #(apply (partial pth transition-graph) %) 
                                       ft-code-l3)
                                  (repeat (list \A)))
        actioned_l3   (flatten directions_l3)
        len           (+ (count actioned_l3))
        npart         (parse-long (re-find #"\d+" code))]
    ;(println code) (println directions) (println directions_l2)
    ;(println directions_l3) (println (flatten actioned_l3))
    (* len npart)))

(comment
  (complexity "379A") ;; [68 379] vs [64 379]
  (->> ["029A" "980A" "179A" "456A" "379A"]
       (map complexity)  ;; ([68 29] [60 980] [68 179] [64 456] [64 379])
       (reduce +))
  (->> "resources/input/2024/input_21" slurp
       s/split-lines
       (map complexity)
       (reduce +)))      ;; 184716

(defn solution-1a 
  [s]
  (->> s s/split-lines (map complexity) (reduce +)))

(comment 
  (solution-1a (slurp "resources/input/2024/input_21"))) ;; 184716
;; A variation is to make maps with the finalized best directions between any two
;; buttons
(defn make-path-map
  [graph nodes]
  (loop [FROM nodes
         TO nodes
         ret {}]
    (if (empty? FROM) (recur nodes (rest TO) ret)
        (if (empty? TO) ret
            (let [from (first FROM) to (first TO)
                  p (into [] (gen-instrucions graph (shortest-path graph from to)))]
              (if (not= from to)
                (recur (rest FROM) TO (assoc-in ret [from to] p))
                (recur (rest FROM) TO ret)))))))

;originally we used two sepparate
(def numpad-paths
  (make-path-map numpad-graph [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A]))

(def dpad-paths
  ;manual fix for < to A not using alternating directions
  (make-path-map arrow-graph [\< \v \> \^ \A]))

; some combinarions are not possible but they shouldn't be present
; in practice
(def all-paths 
  (make-path-map transition-graph [\0 \1 \2 \3 \4
                                   \5 \6 \7 \8 \9
                                   \< \> \v \^ \A]))
; Faster? solution
(defn complexity-lite
  [code]
  (let [;code "179A"
        wstartpos     (str "A" code)
        ft-code       (partition 2 1 wstartpos)
        path_l1       (map #((all-paths (first %)) (second %)) ft-code)
        directions_l1 (interleave path_l1 (repeat (list \A)))
        actioned_l1   (flatten directions_l1)
        ;; begin iteration
        ft-code-l2    (partition 2 1 (cons \A actioned_l1))
        path_l2       (map #(get (all-paths (first %)) (second %) ()) ft-code-l2)
        directions_l2 (interleave path_l2 (repeat (list \A)))
        actioned_l2   (flatten directions_l2)
        ft-code-l3    (partition 2 1 (cons \A actioned_l2))
        path_l3       (map #(get (all-paths (first %)) (second %) ()) ft-code-l3)
        directions_l3 (interleave path_l3 (repeat (list \A)))
        actioned_l3   (flatten directions_l3)
        ;; end interation 
        len (+  (count actioned_l3))
        npart (parse-long (re-find #"\d+" code))]
    ;(println code)
    ;(prn actioned_l1)
    ;(println actioned_l2)
    ;(println actioned_l3)
    ;actioned_l3
    (* len npart)))

(comment
  (->> "resources/input/2024/input_21" slurp
       s/split-lines
       (map complexity-lite)
       (reduce +))) ;; 184716

(defn solution-1b [s]
  (->> s s/split-lines (map complexity-lite) (reduce +)))

(comment
  (solution-1b (slurp "resources/input/2024/input_21"))) ;; 184716
  
;; PART 2
;; Part 2 was based on other solutinons i looked up. mainly
;; https://github.com/erdos/advent-of-code/blob/master/2024/day21.clj
;; from which i took the idea of a unified way of transitioning in both numeric
;;  and arrow pads, and using memoization.
;; Also
;; https://github.com/Maravedis/advent_code/blob/master/src/advent_of_code/2024/21.clj
;; and 
;; https://github.com/rjray/advent-2024-clojure/blob/master/src/advent_of_code/day21.clj 
;; but implementation is significantly different


; Like this better than partitioning in pairs
(defn pair-map
  "maps f to seq in overlapping pairs"
  [f seq] (map f seq (rest seq)))

(def solve
 (memoize 
   (fn [depth sequence]
     (if (zero? depth) (count sequence)
       (->> (cons \A sequence)
            (pair-map #(conj (get-in all-paths [%1 %2] []) \A)) 
            (map (partial solve (dec depth)))
            (reduce +))))))           

(comment 
 (->> ["029A" "980A" "179A" "456A" "379A"]
      (map (partial solve 3))) ;; (68 60 68 64 64)
;; ([68 29] [60 980] [68 179] [64 456] [64 379])
 (->> "resources/input/2024/input_21"
      slurp (s/split-lines)
      (map (fn [s] (* (parse-long (re-find #"\d+" s))
                      (solve 26 s))))
      (reduce +))) ;; 229403562787554


(defn solution-2 
  [s]
  (->> s s/split-lines 
       (map (fn [s] (* (parse-long (re-find #"\d+" s))
                       (solve 26 s))))
       (reduce +)))

(comment 
  (solution-2 (slurp "resources/input/2024/input_21"))) ;; 229403562787554

;; part 1 can be done too with this
(defn solution-1c
  [s]
  (->> s s/split-lines 
       (map (fn [s] (* (parse-long (re-find #"\d+" s))
                       (solve 3 s))))
       (reduce +)))

(comment 
  (solution-1b (slurp "resources/input/2024/input_21"))) ;; 184716

;; =====================================================================
;; Stuff to visually inspect what was going on when i was stuck in part 1.
;; was getting a few codes wrong but most were right.
;; I was comparing the paths produced by a correct solution and mine
;; Also made a lil "game" that shows the states of the different keypads
;;  when imputting directions one by one 'game21.clj'.
(defn reverse-kvs [m]
  (apply assoc {} (mapcat #(reverse %) m)))

(def dpad-travel-map (update-vals arrow-graph reverse-kvs))
(def numpad-travel-map (update-vals numpad-graph reverse-kvs))

(defn instructions-to-presses
  [travel-map instructions]
  (loop [to-follow (seq instructions)
         current \A
         pressed []]
    (if (empty? to-follow) pressed
        (let [this (first to-follow)]
          (if (= this \A)
            (recur (rest to-follow) current (conj pressed current))
            (recur (rest to-follow) (get (travel-map current) this) pressed))))))

(comment
  (println
    (partition-with #(= % \A) 
       (seq "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"))))
; [A](v < < A) [<] (> > ^ A) [A] (A)   (v < A) [v]  (< A) [<]
; [A]  (< v A) [v]     (< A) [<] (A) (> > ^ A) [^]    (A)

(defn instrucions-to-code
  [instructions]
  (->> (seq instructions) 
       (instructions-to-presses dpad-travel-map)
       (instructions-to-presses dpad-travel-map)
       (instructions-to-presses numpad-travel-map)))

(comment
  (println-str (->> "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
                  (instructions-to-presses dpad-travel-map))))

(defn directions
  [graph pair]
  (let [from (first pair)
        to (second pair)]
    ((graph from) to)))

;; (map
;;   #(map (partial directions numpad-graph) %)
;;   (map
;;     (partial partition 2 1)
;;     (filter #(= (count %) 5) (paths numpad-graph \3 \7)))

;;    same until v why do they diverge? they reach 7 in same move tho..    
;;   v<<A>>^AvA^A|v<<A>>^AAv<A<A>>^|AAvAA^<A|>Av<A^>AA<A>Av<A<A>>^AAA<A>vA^A
;;  A>v<Pv>AP>PAP|>v<Pv>APP>vP<Pv>A|PP>PPA^P|AP>vP^APP^PAP>vP<Pv>APPP^PA>PAP
;;  A...^...P.A.P|...^...PP..v.<...|PP.v>..A|.P..>..PP.A.P..>.v...PPP.^..A.P
;;  A.......3...P.......69........8|7.......|P.....89...P........63A......P
;;                                                  *
;; 
;;   <v<A>>^AvA^A|<vA<AA>>^AAvA<^A>|AAvA^A<v|A>^AA<A>A<v<A>A>^AAAvA<^A>A
;;  A^v<Pv>AP>PAP|^vP<PPv>APP>Pv^PA|PP>PAP^v|P>APP^PAP^v<PvP>APPP>Pv^PAP
;;  A...^...P.A.P|..>.v<...PP.v..^.|PP.A.P..|>..PP.A.P...^.v..PPP.>..A.P
;;  A.......3...P.........21......4|7...P...|..89...P........63A......P
;;                                              * <- how come i reach 9 first here
;                                            * <- reaching 'right' first
;;                                      * <- 7 is pushed earlier
;; when n3 points to 7:
;       a2 is on ^ and <
;       a1 is on A and A)))

;; when a2 is on ^ it's faster to go down and right than
;; right twice when a2 is on <

