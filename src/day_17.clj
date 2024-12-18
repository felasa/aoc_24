(ns day-17
  (:require [clojure.string :as s]))

(def exampe-input "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(defn parse-input [s]
  (let [;s exampe-input
        v (s/split-lines s)
        [ra rb rc _ ri] v
        A (parse-long(re-find #"\d+" ra))
        B (parse-long(re-find #"\d+" rb))
        C (parse-long(re-find #"\d+" rc))
        I (mapv parse-long (re-seq #"\d" ri))]
    {:A A :B B :C C :instructions I}))

(defn combo-val [A B C operand]
  (cond (<= 0 operand 3) operand
        (= operand 4) A
        (= operand 5) B
        (= operand 6) C))

(defn compute 
  [registers instructions]
  (loop [pointer 0
         A (registers 0)
         B (registers 1)
         C (registers 2)
         out []]
    ;(println {:pointer pointer :A A :B B :C C :out out})
    (if (>= pointer (count instructions)) {:A A :B B :C C :out out}
      (cond 
        (= (instructions pointer) 0)
        (recur (+ pointer 2)
               (bit-shift-right A ((partial combo-val A B C) (instructions (inc pointer))))
               B C out) 
        (= (instructions pointer) 6)
        (recur (+ pointer 2)
               A 
               (bit-shift-right A ((partial combo-val A B C) (instructions (inc pointer))))
               C out) 
        (= (instructions pointer) 7)
        (recur (+ pointer 2)
               A B 
               (bit-shift-right A ((partial combo-val A B C) (instructions (inc pointer))))
               out)
        (= (instructions pointer) 1)
        (recur (+ pointer 2)
               A 
               (bit-xor B (instructions (inc pointer)))
               C out)
        (= (instructions pointer) 2)
        (recur (+ pointer 2)
               A
               (mod ((partial combo-val A B C) (instructions (inc pointer))) 8)
               C out)
        (= (instructions pointer) 3)
        (if (= A 0) 
          (recur (+ pointer 2) A B C out)
          (recur (instructions (inc pointer)) A B C out))
        (= (instructions pointer) 4)
        (recur (+ pointer 2) 
               A
               (bit-xor B C)
               C out)
        (= (instructions pointer) 5)
        (recur (+ pointer 2) A B C
               (conj out 
                     (mod ((partial combo-val A B C) (instructions (inc pointer))) 8)))))))

(comment
  (compute [0 0 9] [2 6]) ;; {:A 0, :B 1, :C 9, :out []}
  (compute [10 0 0] [5 0 5 1 5 4]) ;; {:A 10, :B 0, :C 0, :out [0 1 2]}
  (compute [2024 0 0] [0 1 5 4 3 0])  ;; {:A 0, :B 0, :C 0, :out [4 2 5 6 7 7 7 7 3 1 0]}
  (compute [0 29 0] [1 7]) ;; {:A 0, :B 26, :C 0, :out []}
  (compute [0 2024 43690] [4 0])) ;; {:A 0, :B 44354, :C 43690, :out []}

(defn solution-1 [s]
  (let [state (parse-input s)
        A (state :A)
        B (state :B)
        C (state :C)
        instructions (state :instructions)]
    (apply str (interpose "," (:out (compute [A B C] instructions))))))

(comment 
  (compute [729 0 0] [0 1 5 4 3 0]) ;; {:A 0, :B 0, :C 0, :out [4 6 3 5 6 3 5 2 1 0]}
  (solution-1 exampe-input) ;; "4,6,3,5,6,3,5,2,1,0"
  (compute [27575648 0 0] [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0]) ;; {:A 0, :B 1, :C 0, :out [5 4 3 6 3 0 4 1 1]}
  (parse-input (slurp "resources/input/input_17"))
;; {:A 27575648,
;;  :B 0,
;;  :C 0,
;;  :instructions [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0]}
  (solution-1 (slurp "resources/input/input_17")))  ;; "1,7,2,1,4,1,5,4,0"

(defn s1 []
  (solution-1 (slurp "resources/input/input_17")))  ;; "1,7,2,1,4,1,5,4,0"


;; PART 2

;; (mod x 8) = 2 ==> 3|x = 010

(comment 
  (take 100 (map (fn [i] (:out (compute [i 0 0] [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0])))
                 (map #(+ 35184372088832 %) (range)))))

(def pow 
  (memoize 
    (fn step [n power]
        (cond (= power 0) 1
              (= power 1) n
              (> power 1) (* n (step n (dec power)))))))

(comment 
  (take 16 (map (fn [i] (:out (compute [i 0 0] [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0])))
                (range (bit-shift-left  1 (* 3 15)) (bit-shift-left  1 (* 3 16)) 
                       (pow 8 0))))
  (->> 
    (range (bit-shift-left  1 (* 3 15)) (bit-shift-left  1 (* 3 16)) (pow 8 10))
    (map (fn [i] [i (:out (compute [i 0 0] [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0]))]))
    (filter (fn [[idx out]] (and (= (out 15) 0) (= (out 14) 3) (= (out 13) 3))))
    (second)))

;; every 8^10 the 14th digit changes
;; in general every 8^k the k+4 digit changes
;; so for int between (bit-shift-left  1 (* 3 15)) (bit-shift-left  1 (* 3 16))
;; increase in steps of 8^10 until a 3 is found in the 14th
;; then increase in steps of 8^9 until 0 is found in the 13th
;; if none, return iterating 8^10 from where we left off
;; if found, increase in steps of 8^8 until 5 is found on the 12th 
;; and so on
;; [0 8 16 32 ..]
;; [  8 10 16 ]

(comment 
  ; Not right but cant figure out why
  (let [target-result [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0]
        MAX (bit-shift-left 1 (* 3 16))]
   (loop [a    (bit-shift-left 1 (* 3 15))
          idx-check 13
          maxl (+ a (pow 8 (inc idx-check)))]
     (if (< idx-check 0) a
       (let [out (:out (compute (vector a 0 0) target-result))]
         (if (= (get out idx-check) (get target-result idx-check))
           (recur a 
                  (dec idx-check)
                  (+ a (pow 8 idx-check)))
           (if (< a maxl)
             (recur (+ a (pow 8 idx-check))
                    idx-check
                    maxl)
             (do
               (println idx-check a)
               (recur maxl
                      (inc idx-check)   
                      (+ maxl (pow 8 (+ idx-check 2))))))))))))

;; Solved manually changing values :|
(comment 
  (let [target-result [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0]
        MAX (bit-shift-left 1 (* 3 16))
        a   (+ (bit-shift-left 1 (* 3 15)) 
               (* 3 (pow 8 13))
               (* 5 (pow 8 12))
               (* 5 (pow 8 11))
               (* 1 (pow 8 10))
               (* 0 (pow 8  9))
               (* 0 (pow 8  8))
               (* 0 (pow 8  7))
               (* 5 (pow 8  6))
               (* 1 (pow 8  5))
               (* 3 (pow 8  4))
               (* 6 (pow 8  3))
               (* 7 (pow 8  2))
               (* 6 (pow 8  1)))
        idx-check 0
        maxl (+ a (pow 8 (inc idx-check)))
        a (+ a (* 4 (pow 8 idx-check)))
        out (:out (compute (vector a 0 0) target-result))]
    (println (< a maxl)) out))

(+ (bit-shift-left 1 (* 3 15)) 
   (* 3 (pow 8 13))
   (* 5 (pow 8 12))
   (* 5 (pow 8 11))
   (* 1 (pow 8 10))
   (* 0 (pow 8  9))
   (* 0 (pow 8  8))
   (* 0 (pow 8  7))
   (* 5 (pow 8  6))
   (* 1 (pow 8  5))
   (* 3 (pow 8  4))
   (* 6 (pow 8  3))
   (* 7 (pow 8  2))
   (* 6 (pow 8  1))
   (* 4 (pow 8  0)))  ;; 37221261688308

(:out (compute [37221261688308 0 0] [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0]))
(:out (compute [38878582193652 0 0] [2 4 1 2 7 5 4 1 1 3 5 5 0 3 3 0]))

