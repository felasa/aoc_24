(ns day-07
  (:require [clojure.string :as s]))

(def example-iput 
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-input [s]
  (->> s
      s/split-lines
      (map #(s/split % #": +| +"))
      (map (partial map parse-long))))

(def example-data (parse-input example-iput))

(defn apply-op [coll op] (cons (apply op (take 2 coll)) (drop 2 coll)))

(defn apply-ops
  [coll ops]
  (loop [to-do ops 
         ret coll]
    (if (empty? to-do) ret
      (recur (rest to-do) (apply-op ret (first to-do))))))

(defn append
  [a0 b0]
  (cond 
    (and (coll? a0) (coll? b0)) (into a0 b0)
    (coll? a0) (conj a0 b0)
    (coll? b0) (conj b0 a0)
    :else (vector a0 b0)))

(defn cross 
  ([coll1] (map vector coll1))
  ([coll1 coll2] (for [e1 coll1 e2 coll2] (append e1 e2))) 
  ([coll1 coll2 & colls] (apply (partial cross (cross coll1 coll2)) colls)))

(comment 
  (apply cross (repeat 2 [+ *])))
;; (["+" "+" "+"]
;;  ["+" "+" "*"]
;;  ["+" "*" "+"]
;;  ["+" "*" "*"]
;;  ["*" "+" "+"]
;;  ["*" "+" "*"]
;;  ["*" "*" "+"]
;;  ["*" "*" "*"])

(defn find-ops
  [target seq]
  (let [n-ops (dec (count seq))
        op-list (apply cross (repeat n-ops [+ *]))]
    (loop [to-do op-list]
      (if (empty? to-do) nil
        (let [applied (apply-ops seq (first to-do))]
          ;(println applied)
          (if (= target (first applied)) 
              true
              (recur (rest to-do)))))))) 
        
(defn solution-1 
  [data]
  (reduce + (map first (filter (fn [l] (find-ops (first l) (rest l))) data))))

(comment 
  (solution-1 (parse-input example-iput)) ;; 3749
  (solution-1 (parse-input (slurp "resources/input/2024/input_07")))) ;; 2654749936343

(defn s1 []
  (solution-1 (parse-input (slurp "resources/input/2024/input_07"))))

;; PART 2

(defn paste
  [n1 n2]
  (parse-long (str n1 n2)))

(defn find-ops-pt2
  [target seq]
  (let [n-ops (dec (count seq))
        op-list (apply cross (repeat n-ops [+ * paste]))]
    (loop [to-do op-list]
      (if (empty? to-do) nil
        (let [applied (apply-ops seq (first to-do))]
          ;(println applied)
          (if (= target (first applied)) 
              true
              (recur (rest to-do)))))))) 

;; slow but tolerable
(defn solution-2 
  [data]
  (->> data
       (filter (fn [l] (find-ops-pt2 (first l) (rest l))))
       (map first)
       (reduce +))) 
      
(comment
  (solution-2 example-data) ;; 11387
  (solution-2 (parse-input (slurp "resources/input/2024/input_07")))) ;; 124060392153684

(defn s2 []
  (solution-2 (parse-input (slurp "resources/input/2024/input_07"))))
