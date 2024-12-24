(ns day-24
  (:require [clojure.string :as string]))

(def example-input "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj")

(def op2fun {"OR" bit-or "AND" bit-and "XOR" bit-xor})

(defn parse-input 
  [s]
  (let [[initial topology] 
        (->> s string/split-lines (split-with (partial not= "")))
        t0 (update-vals 
             (reduce #(apply (partial assoc %1) %2 ) {} 
                     (map #(string/split  % #": ")  initial)) 
             (comp parse-long))
        t1 (update-vals 
             (reduce (fn [l r] (assoc l (r 4) (list (r 1) (r 0) (r 2) ))) {}
                   (map #(string/split % #" | -> ") (rest topology)))
             #(assoc (vec %) 0 (op2fun (first %))))]
    {:init t0 :topology t1}))  

(comment 
  ; thinking it would be better to do a whole pass at a time
  ; not-implemented tho
  (defn update-once 
    [state]
    (loop [resolved (transient (:init state))
           to-do (:topology state)
           unresolved {}]
      (if (empty? unresolved) {:init resolved :topology unresolved}
        (let [[head & tail] to-do
              [op left right] head])))))
        
(defn run-machine
  "Propagates all the states through rules, if badly defined could run forever"
  [state]
  (loop [unresolved (vec (:topology state))
         resolved (:init state)]
    (if (empty? unresolved) resolved
      (let [[head & tail] unresolved
            [wire origin] head
            [op left right] origin]
        (if (and (resolved left) (resolved right))
          (recur (vec tail) (assoc resolved wire (op (resolved left) (resolved right))))
          (recur (conj (vec tail) head) resolved))))))

(defn solution-1 
  [s]
  (->> s parse-input run-machine
       (filter #(= "z" (subs (key %) 0 1)))
       sort
       (reduce (fn [l r] (str (r 1) l)) "")
       (str "2r")
       (read-string)))  
       
(comment 
  (solution-1 example-input) ;; 2024
  (solution-1 (slurp "resources/input/input_24"))) ;; 51657025112326

(defn s1 []
  (solution-1 (slurp "resources/input/input_24")))
 
