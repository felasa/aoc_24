(ns y2023.day-16
  (:require [clojure.string :as s]))

(def example-input ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defn propagate 
  [contraption visited start direction]
  (loop [Q (vector [start direction])
         visited visited]
    (let [[current direction :as head] (peek Q)
          c (get-in contraption current)]
      ;(prn {:h head :c c})
      (if (visited head) (recur (pop Q) visited)
        (if head 
          (case c
            \. (recur (conj (pop Q) [(mapv + current direction) direction])
                      (conj visited head))
            \- (if (#{[0 1] [0 -1]} direction)
                 (recur (conj (pop Q) [(mapv + current direction) direction])
                        (conj visited head))
                 (recur (into (pop Q)
                              (map #(vector (mapv + current %) %) [[0 -1] [0 1]]))
                        (conj visited head)))
            \| (if (#{[1 0] [-1 0]} direction)
                 (recur (conj (pop Q) [(mapv + current direction) direction])
                        (conj visited head))
                 (recur (into (pop Q) (map #(vector (mapv + current %) %)
                                           [[-1 0] [1 0]]))
                        (conj visited head)))
            \\ (cond (= direction [0 -1])
                     (recur (conj (pop Q) [(mapv + current [-1 0]) [-1 0]])
                            (conj visited head))
                     (= direction [0 1])
                     (recur (conj (pop Q) [(mapv + current [1 0]) [1 0]])
                            (conj visited head))
                     (= direction [-1 0])
                     (recur (conj (pop Q) [(mapv + current [0 -1]) [0 -1]])
                            (conj visited head)) 
                     (= direction [1 0])
                     (recur (conj (pop Q) [(mapv + current [0 1]) [0 1]])
                            (conj visited head)))
            \/ (cond (= direction [0 -1])
                     (recur (conj (pop Q) [(mapv + current [1 0]) [1 0]])
                            (conj visited head))
                     (= direction [1 0])
                     (recur (conj (pop Q) [(mapv + current [0 -1]) [0 -1]])
                            (conj visited head))
                     (= direction [-1 0])
                     (recur (conj (pop Q) [(mapv + current [0 1]) [0 1]])
                            (conj visited head))
                     (= direction [0 1])
                     (recur (conj (pop Q) [(mapv + current [-1 0]) [-1 0]])
                            (conj visited head)))
            (recur (pop Q) visited))
          visited)))))

(defn solution-1 [s]
  (let [contraption (->> s s/split-lines (mapv vec))]
    (->> (propagate contraption #{} [0 0] [0 1])
         (group-by first)
         (count))))
    
(comment 
  (solution-1 example-input) ;; 46
  (solution-1 (slurp "resources/input/2023/input_16"))) ;; 7951

;; PART 2
;just brute force
(defn entry-points
  [height width]
  (concat 
    (map vector (map vector (repeat 0) (range width)) (repeat [1 0]))
    (map vector (map vector (repeat (dec height)) (range width)) (repeat [-1 0]))
    (map vector (map vector (range height) (repeat (dec width))) (repeat [0 -1]))
    (map vector (map vector (range height) (repeat 0)) (repeat [0 1]))))

(defn p-n-c 
  [contraption start direction]
  (count (distinct (map first (propagate contraption #{} start direction)))))

(defn max-energy [contraption]
  (let [H (count contraption) W (count (contraption 0))
        eps (entry-points H W)
        n-ergized (into [] (map (partial apply p-n-c contraption) eps))]
    (n-ergized (apply max-key n-ergized
                      (range (count n-ergized))))))

(defn solution-2 [s]
  (->> s s/split-lines
       (mapv vec)
       max-energy))

(comment 
  (solution-2 example-input) ;; 51
  (solution-2 (slurp "resources/input/2023/input_16")))  ;; 8148
