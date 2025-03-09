(ns y2023.day-14
  (:require [clojure.string :as s]
            [y2023.day-13 :refer [transpose]]))

(def example-input "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn tilt-start
  [v]
  (let [W (count v)]
    (loop [base (transient (vec v)) 
           fall-idxs []
           idx 0]
        ;(println {:pk (get base 0) :fi fall-idxs, idx 0})
        (if (< idx W)
          (case (base idx)
           \O (if-let [fall-idx (get fall-idxs 0)]
                 (recur (assoc! base fall-idx \O idx \.) (subvec fall-idxs 1) idx)
                 (recur base fall-idxs (inc idx)))
           \. (recur base (conj fall-idxs idx) (inc idx))
           \# (recur base [] (inc idx)))
          (persistent! base)))))

(defn solution-1
  [s]
  (->> s s/split-lines transpose
       (mapv tilt-start)
       transpose
       (reverse)
       (map-indexed (fn [idx item] (* (inc idx) (count (remove #{\. \#} item)))))
       (reduce +)))

(comment
  (solution-1 example-input) ;; 136
  (solution-1 (slurp "resources/input/2023/input_14"))) ;; 111339

;;; PART 2 

(defn tilt-end
  [v]
  (vec (reverse (tilt-start (vec (reverse v))))))   

(defn tilt-north 
  [sq]
  (transpose (mapv tilt-start (transpose sq))))

(defn tilt-east
  [sq]
  (mapv tilt-end sq))

(defn tilt-south
  [sq]
  (transpose (mapv tilt-end (transpose sq))))

(defn tilt-west
  [sq]
  (mapv tilt-start sq))

(defn tilt-cycle [sq]
  (-> sq tilt-north
      tilt-west tilt-south tilt-east))

;; Assumption is that sytem will reach a stable point where cycles are well.. cyclic
;;  with some period
;;  this was confirmed by inspection but don't know how to prove it can be assumed
;; Find a stable point where a cycle is found.
;; Then return the cycle period along with the stable point
(defn find-cycle
  [limit state]
  (loop [iters (take limit (rest (iterate tilt-cycle state)))
         i 1]
    (if-let [this (first iters)]
      (if (= this state) i
        (recur (rest iters) (inc i)))
      nil)))
         
(defn find-stable 
  [dish]
  (loop [n (+ (count dish) (count (first dish)))]
    (let [state (nth (iterate tilt-cycle dish) n)]
      (if-let [cycle (find-cycle 100 state)] 
        [n cycle]
        (recur (bit-shift-left n 1))))))

;; 1e9 = period*k + (rem 1e9 period) 
;; k: period*k >= stable
(defn solution-2
  [s]
  (let [dish  (->> s s/split-lines) 
        [n-stable period] (find-stable dish)
        equivalent (+ (* (inc (quot n-stable period))
                         period)
                      (rem 1e9 period)) 
        eq-dish (nth (iterate tilt-cycle dish) equivalent)]
    (->> eq-dish
       reverse
       (map-indexed (fn [idx item] (* (inc idx) (count (remove #{\. \#} item)))))
       (reduce +))))

(comment
  (solution-2 example-input) ;; 64
  (solution-2 (slurp "resources/input/2023/input_14"))) ;; 93736
        
