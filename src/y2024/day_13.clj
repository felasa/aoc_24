(ns day-13)

(def example-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defn proc-input [s]
  (let [;s example-input
        reg-matches (re-seq #"X\+(\d+), Y\+(\d+)|X=(\d+), Y=(\d+)" s)
        xys (for [lm reg-matches
                  :let [x ((fnil parse-long "") (lm 1)) y ((fnil parse-long "") (lm 2))]
                  :when (and x y)]
               [x y])
        ts  (for [lm reg-matches
                  :let [tx ((fnil parse-long "") (lm 3)) ty ((fnil parse-long "") (lm 4))]
                  :when (and tx ty)]
              [tx ty])]
     (map conj (map (partial into []) (partition 2 xys)) ts)))

(comment 
  (proc-input example-input))

; is it really optimal? just finds the solution with maximal B presses
; a system of 2 eqs. with two variables has only 1 solution anyway...
; so any solution should be the optimal unless indefinite or non-integer
(defn optimal-solution 
  [a b target]
  (let [[a-delta-x a-delta-y] a
        [b-delta-x b-delta-y] b
        [x y] target]
    (loop [nb (int (min (/ x b-delta-x) (/ y b-delta-y)))
           na 0]
     ;(println {:B nb :A na})
     (let [sx (+ (* nb b-delta-x) (* na a-delta-x))
           sy (+ (* nb b-delta-y) (* na a-delta-y))]
      (cond (< nb 0) nil
            (and (= x sx) (= y sy)) [na nb]
            (or  (> sx x) (> sy y)) (recur (dec nb) na)
            (or  (< sx x) (< sy y)) (recur nb (inc na)))))))

(defn solution-1
  [s]
  (->> s proc-input 
       (map (partial apply optimal-solution))
       (map (fnil (fn [sol] (+ (sol 1) (* 3 (sol 0)))) [0 0]))     
       (reduce +)))
  
(comment 
  (solution-1 example-input) ;; 480
  (solution-1 (slurp "resources/input/2024/input_13"))) ;; 37686

;; PART 2

;; ok stopped being lazy and wrote a solver
;; unsure if this handles all cases well (indeterminate, or some differences being 0) 
;; but worked for all the puzzle ones
;; only sanity check is for non-integer solutions
(defn solver
  [[x_a y_a] [x_b y_b] [X Y]]
  (let [B (/ (+ (* X (- y_a x_a)) (* x_a (- X Y)))
             (+ (* x_a (- x_b y_b)) (* x_b (- y_a x_a))))
        A (/ (- X (* B x_b)) x_a)]
    (if (and (= (long A) A) (= (long B) B)) [A B] nil)))

;shouldve done this from start, its fun looping tho
(defn solution-1b
  [s]
  (->> s proc-input
       (map (partial apply solver))
       (map (fnil (fn [sol] (+ (sol 1) (* 3 (sol 0)))) [0 0]))     
       (reduce +)))

(comment 
  (solution-1b example-input)  ;; 480
  (solution-1b (slurp "resources/input/2024/input_13")))  ;; 37686

(defn add-to-target
  [input]
  (let [[A B [X Y]] input]
    [A B [(+ X 10000000000000) (+ Y 10000000000000)]]))

(defn solution-2
  [s]
  (->> s proc-input
       (map add-to-target) 
       (map (partial apply solver))
       (map (fnil (fn [sol] (+ (sol 1) (* 3 (sol 0)))) [0 0]))     
       (reduce +)))

(comment 
  (solution-2 example-input) ;; 875318608908
  (solution-2 (slurp "resources/input/2024/input_13"))) ;; 77204516023437

(defn s2 []
  (solution-2 (slurp "resources/input/2024/input_13"))) 

