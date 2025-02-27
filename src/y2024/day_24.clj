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
  (parse-input example-input)
  (parse-input (slurp "resources/input/2024/input_24"))
  ; thinking it would be better to do a whole pass at a time
  ; not-implemented tho
  (defn update-once 
    "not implemented"
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
          (recur (vec tail)
                 (assoc resolved wire (op (resolved left) (resolved right))))
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
  (solution-1 (slurp "resources/input/2024/input_24"))) ;; 51657025112326

(defn s1 []
  (solution-1 (slurp "resources/input/2024/input_24")))

;; PART 2
;; Part 2 was done by visually inspecting the circuit. I looked for other solutions 
;;   and I didnt find anyone that did a programatic one.
;; So this was about identifying the pattern a binary adder has, probing in which bits the adder 
;;   goes wrong and then inspecting the wires for where it's wrong and finally querying to find the 
;;   replacement wires.

;; First we probe the circuit for specific positions where it goes wrong
(defn lpad
  "If npm is down you can use this to pad a string to the left up
  to n characters filled with the character c"
  [n c x]
  (let [sx (count x)]
    (if (>= sx n) x
      (str (apply str (repeat (- n sx) c)) x))))

(defn to-45-bit 
  "Encode x as a 45 bit binary string"
  [x]
  (lpad 45 "0" (Long/toString x 2)))

(comment 
  (to-45-bit (bit-shift-left 1 19)) ;; "000000000000000000000000010000000000000000000"
  (to-45-bit 800)) ;; "000000000000000000000000000000000001100100000"

(defn apply-values
  "Apply the circuit to numbers x and y"
  [circuit x y]
  (let [bx (to-45-bit x)
        by (to-45-bit y)
        init (zipmap (concat (map #(str "x" (format "%02d" %)) (range 44 -1 -1))
                             (map #(str "y" (format "%02d" %)) (range 44 -1 -1)))
                     (concat (map #(parse-long (str %)) bx)
                             (map #(parse-long (str %)) by)))
        in {:topology circuit :init init}]
    (->> in 
         run-machine 
         (filter #(= (first (key %)) \z))
         (sort (fn [x y] (compare y x)))
         (map val)
         (apply str "2r")
         (read-string))))

(comment 
  (apply-values (:topology (parse-input (slurp "resources/input/2024/input_24")))
                1 1)) ;; 2
    
(defn bits-wrong
  "Checks sums of powers of 2 to find where the adder circuit goes wrong"
  [topology]
  (let [wx (loop [px (range 0 45)
                  ret {:x []}]
             (if (empty? px) ret
               (if (= (apply-values topology (bit-shift-left 1 (first px)) 0) 
                      (+ (bit-shift-left 1 (first px)) 0))
                 (recur (rest px) ret)
                 (recur (rest px) (assoc ret :x (conj (:x ret) (first px)))))))
        wy (loop [px (range 0 45) ret {:y []}]
             (if (empty? px) ret
               (if (= (apply-values topology (bit-shift-left 1 (first px)) 0) 
                      (+ (bit-shift-left 1 (first px)) 0))
                 (recur (rest px) ret)
                 (recur (rest px) (assoc ret :y (conj (:y ret) (first px)))))))]
    (merge wx wy)))
  
(comment 
  (bits-wrong (:topology (parse-input (slurp "resources/input/2024/input_24"))))) ;; {:x [5 9 15 30], :y [5 9 15 30]}

; This wasnt really needed but sheds light on wether it was the carry or 
; original position that was wrong
(defn bits-wrong-2
  "like bits-wrong but also adds non zero stuff"
  [topology]
  (let [wx (loop [px (range 0 45)
                  ret {:x []}]
             (if (empty? px) ret
               (if (= (apply-values topology
                                    (bit-shift-left 1 (first px)) 
                                    (bit-shift-left 1 (first px))) 
                      (+ (bit-shift-left 1 (first px)) 
                         (bit-shift-left 1 (first px))))
                 (recur (rest px) ret)
                 (recur (rest px) (assoc ret :x (conj (:x ret) (first px)))))))]
    wx))
  
;; Next part was about learning how adder circuit works fortunately
;; the problem does not deviate from a straighforward adder 

;; the adder is basically:
;; digit at position zk = (xor (xor yk xk) carry?) 
;; where carry? expands into the less significant digits

;zk (xor (xor yk zk) (or (and yk-1 xk-1) (and (xor yk-1 xk-1) (and yk-2 xk-2)) ...)
;; basically zk is 0 if yk,xk=1 AND no carry over or 
;;   ONLY one of yk xk is one (xor true) and theres a carry over
;; is 1 if ONLY one of xk yk is 1 and theres no carry over. or both yk xk are zero and carry
;; and so on.

; var it for convenience
(def topo (:topology (parse-input (slurp "resources/input/2024/input_24"))))

;this is how two good positions look:
(topo "z02") ;; [#function[clojure.core/bit-xor] "rtc" "fkn"]
,(topo "fkn") ;; [#function[clojure.core/bit-xor] "x02" "y02"]
,(topo "rtc") ;; [#function[clojure.core/bit-or] "cpp" "pss"]
, (topo "cpp") ;; [#function[clojure.core/bit-and] "y01" "x01"]
, (topo "pss") ;; [#function[clojure.core/bit-and] "jfb" "jjj"]
,   (topo "jfb") ;; [#function[clojure.core/bit-and] "y00" "x00"]
,   (topo "jjj") ;; [#function[clojure.core/bit-xor] "y01" "x01"]
;; (xor (or (and y01 x01) (and (and y00 x00) (xor y01 x01)))
;       (xor x02 y02))
(topo "z04") ;; [#function[clojure.core/bit-xor] "cwp" "rsk"]
, (topo "cwp") ;; [#function[clojure.core/bit-xor] "y04" "x04"]
, (topo "rsk") ;; [#function[clojure.core/bit-or] "ttc" "vkp"]
,   (topo "ttc") ;; [#function[clojure.core/bit-and] "x03" "y03"]
,   (topo "vkp") ;; [#function[clojure.core/bit-and] "fhp" "psp"]
,     (topo "fhp") ;; [#function[clojure.core/bit-xor] "y03" "x03"]
,     (topo "psp") ;; [#function[clojure.core/bit-or] "dbr" "vrb"]
,       (topo "vrb") ;; [#function[clojure.core/bit-and] "x02" "y02"]
,       (topo "dbr") ;; [#function[clojure.core/bit-and] "rtc" "fkn"]
,         (topo "fkn") ;; [#function[clojure.core/bit-xor] "x02" "y02"]
,         (topo "rtc") ;; [#function[clojure.core/bit-or] "cpp" "pss"]
,           (topo "cpp") ;; [#function[clojure.core/bit-and] "y01" "x01"]
,           (topo "pss") ;; [#function[clojure.core/bit-and] "jfb" "jjj"]
,             (topo "jfb") ;; [#function[clojure.core/bit-and] "y00" "x00"]
,             (topo "jjj") ;; [#function[clojure.core/bit-xor] "y01" "x01"]

;; so the pattern is clear. always starts with an xor, the operands of it has 
;;   to be another xor of the x,y in the same position and the other an or
;; then there's a pattern: and and xor or and and xor or .. and and and xor

;; first digit that has problem is the fifth:
(topo "z05") ;; [#function[clojure.core/bit-or] "rnk" "mkq"] ;should be an xor so z05 has to be swapped
, (topo "mkq") ;; [#function[clojure.core/bit-and] "y05" "x05"] ;this should be xor. z05->wwm?
, (topo "rnk") ;; [#function[clojure.core/bit-and] "tsw" "wwm"] ;or?
,   (topo "wwm") ;; [#function[clojure.core/bit-xor] "x05" "y05"] ;sequence seems right from here. swap z05 with the key for (xor tsw wwm): "hdt" found below
,   (topo "tsw") ;; [#function[clojure.core/bit-or] "dmh" "dsn"]
,     (topo "dsn") ;; [#function[clojure.core/bit-and] "x04" "y04"]
,     (topo "dmh") ;; [#function[clojure.core/bit-and] "cwp" "rsk"]
,       (topo "cwp") ;; [#function[clojure.core/bit-xor] "y04" "x04"]
,       (topo "rsk") ;; [#function[clojure.core/bit-or] "ttc" "vkp"]
,         (topo "ttc") ;; [#function[clojure.core/bit-and] "x03" "y03"]
,         (topo "vkp")  ;; [#function[clojure.core/bit-and] "fhp" "psp"]
,           (topo "fhp")  ;; [#function[clojure.core/bit-xor] "y03" "x03"]
,           (topo "psp") ;; [#function[clojure.core/bit-or] "dbr" "vrb"]
,             (topo "vrb") ;; [#function[clojure.core/bit-and] "x02" "y02"]
,             (topo "dbr") ;; [#function[clojure.core/bit-and] "rtc" "fkn"] 
,               (topo "fkn") ;; [#function[clojure.core/bit-xor] "x02" "y02"]
,               (topo "rtc") ;; [#function[clojure.core/bit-or] "cpp" "pss"]
,                 (topo "cpp") ;; [#function[clojure.core/bit-and] "y01" "x01"]
,                 (topo "pss") ;; [#function[clojure.core/bit-and] "jfb" "jjj"]
,                   (topo "jfb") ;; [#function[clojure.core/bit-and] "y00" "x00"]
,                   (topo "jjj") ;; [#function[clojure.core/bit-xor] "y01" "x01"]

(filter #(= (val %) [bit-xor "tsw" "wwm"]) topo) ;; (["hdt" [#function[clojure.core/bit-xor] "tsw" "wwm"]])
(first topo)

;; SWAP z05 and hdt
(bits-wrong (assoc topo
                   "z05" (topo "hdt")
                   "hdt" (topo "z05"))) ;; {:x [9 15 30], :y [9 15 30]}
; 5th digit working now

; checking position 9
(topo "z09") ;; [#function[clojure.core/bit-and] "x09" "y09"] ;should be an xor
;; an xor that has the xor of x09 and y09 and something else  
(filter #(= (val %) [bit-xor "y09" "x09"]) topo) ;; (["wqr" [#function[clojure.core/bit-xor] "y09" "x09"]])
(filter #(and (= ((val %) 0) bit-xor)
              (some #{"wqr"} (subvec (val %) 1))) topo) ;; (["gbf" [#function[clojure.core/bit-xor] "vkd" "wqr"]])
(topo "vkd") ;; [#function[clojure.core/bit-or] "wvc" "hrv"] ;;seems right 
;so z09 <-> gbf?
(bits-wrong (assoc topo
                   "z05" (topo "hdt")
                   "hdt" (topo "z05")
                   "z09" (topo "gbf")
                   "gbf" (topo "z09"))) ;; {:x [15 30], :y [15 30]}
;; digit 9 working right now

;; checking 15
(topo "z15") ;; [#function[clojure.core/bit-xor] "mht" "fgc"]
, (topo "mht") ;; [#function[clojure.core/bit-and] "y15" "x15"] ;wrong should be xor
, (topo "fgc") ;; [#function[clojure.core/bit-or] "bkm" "bwr"]
;; again looking for an xor that contains an xor with y15 and x15
(filter #(= (val %) [bit-xor "y15" "x15"]) topo) ;; (["jgt" [#function[clojure.core/bit-xor] "y15" "x15"]])
(filter #(and (= ((val %) 0) bit-xor)
              (some #{"jgt"} (subvec (val %) 1))) topo)  ;; ()
;; none so swap straight mht with jgt?
(bits-wrong (assoc topo
                   "z05" (topo "hdt")
                   "hdt" (topo "z05")
                   "z09" (topo "gbf")
                   "gbf" (topo "z09")
                   "mht" (topo "jgt")
                   "jgt" (topo "mht"))) ;; {:x [30], :y [30]}
;; 15 fixed

;checking 30
(topo "z30") ;; [#function[clojure.core/bit-and] "dpr" "nvv"] ;wrong should be and xor 
;; an xor that conintains an xor with y30 and x30 and an or
(filter #(= (val %) [bit-xor "y30" "x30"]) topo) ;; (["dpr" [#function[clojure.core/bit-xor] "y30" "x30"]])
(filter #(and (= ((val %) 0) bit-xor)
              (some #{"dpr"} (subvec (val %) 1))) topo) ;; (["nbf" [#function[clojure.core/bit-xor] "dpr" "nvv"]])
(topo "nvv") ;; [#function[clojure.core/bit-or] "jnk" "hdf"] 
;so z30 <-> nbf?
(bits-wrong (assoc topo
                   "z05" (topo "hdt")
                   "hdt" (topo "z05")
                   "z09" (topo "gbf")
                   "gbf" (topo "z09")
                   "mht" (topo "jgt")
                   "jgt" (topo "mht")
                   "z30" (topo "nbf")
                   "nbf" (topo "z30"))) ;; {:x [], :y []}
;all fixed.

(defn solution-2 []
  (apply str (interpose "," (sort ["z05" "hdt" "z09" "gbf" "mht" "jgt" "z30" "nbf"])))) 

(comment
  (solution-2)) ;; "gbf,hdt,jgt,mht,nbf,z05,z09,z30"

;; there could be a program that finds the swaps but it would hang 
;; on the fact that the circuit is standard and has no redundant or cancelling components
;; visual inspection worked because theres few swaps and they're all up high up 

