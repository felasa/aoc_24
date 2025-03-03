(ns y2023.day-11
  (:require [clojure.string :as s]))

(def example-input "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(def example-data 
  (s/split-lines example-input))

(defn expanding? 
  [coll]
  (every? #{\.} coll))

(defn row-at [width row]
  (mapv #(vector row %) (range width)))

(defn column-at [height column]
  (mapv #(vector % column) (range height)))
  
(comment
  (expanding? [\. \. \.]) ;; true
  (expanding? [\. \# \.]) ;; false
  (row-at 5 3) ;; ([3 0] [3 1] [3 2] [3 3] [3 4])
  (column-at 5 3)) ;; ([3 0] [3 1] [3 2] [3 3] [3 4])

(defn find-expanding
  [image]
  (let [H (count image)
        W (count (image 0))
        exp-cols (filterv #(expanding? (map (partial get-in image) (column-at H %)))
                         (range W))
        exp-rows (filterv #(expanding? (map (partial get-in image) (row-at W %)))
                         (range H))]
      {:cols exp-cols :rows exp-rows}))

(comment
  (find-expanding example-data)) ;; {:cols (2 5 8), :rows (3 7)}

(defn expand-at 
  ([v position]
   (let [left (subvec v 0 (inc position))
         right (subvec v position)]
     (into left right)))
  ([v position & positions]
   (let [acc-positions (map +
                            (distinct (cons position positions))
                            (range (inc (count positions))))
         return (reduce (fn [l r] (expand-at l r)) v acc-positions)]
     return)))


(comment 
  (expand-at [0 1 2 3 4 5] 3)  ;; [0 1 2 3 3 4 5]
  (expand-at [[0 0] [1 1] [2 2] [3 3]] 2)  ;; [[0 0] [1 1] [2 2] [2 2] [3 3]]
  (expand-at [0 1 2 3 4 5] 0 2  4))
  
(defn expand-image
  ([image] (expand-image image (find-expanding image)))
  ([image expanding]
   (let [;H (count image) W (count (image 0))
         exp-cols (:cols expanding)
         exp-rows (:rows expanding)
         h-expanded (mapv #(apply expand-at (vec %) exp-cols) image)
         v-expanded (apply expand-at h-expanded exp-rows)]
                        
      v-expanded)))

(comment 
  (expand-image example-data)
  (expand-image example-data {:cols [2 5 8] :rows [3 7]})) 

(defn find-galaxies 
  [image]
  (let [H (count image) W (count (image 0))]
    (loop [row 0 col 0
           ret []]
        (cond (>= row H) ret
              (>= col W) (recur (inc row) 0 ret)
              :else (if (#{\#} (get-in image [row col]))
                      (recur row (inc col) (conj ret [row col]))
                      (recur row (inc col) ret))))))       

(comment 
  (find-galaxies (expand-image example-data)))

(defn l1-dist 
  [[r1 c1] [r2 c2]]
  (+ (abs (- r1 r2)) (abs (- c1 c2))))

(defn solution-1 
  [s]
  (->> (let [image (s/split-lines s)
             gs (find-galaxies (expand-image image))]
         (for [i (range (count gs))
               j (range (inc i) (count gs))
               :let [d (l1-dist (gs i) (gs j))]]
           d))
       (reduce +)))

(comment
  (solution-1 example-input) ;; 374
  (solution-1 (slurp "resources/input/2023/input_11"))) ;; 9556712

;; PART 2
;; if empty row/col between galaxies increase distance by (1e6 - 1) * (n expansions)
(defn solution
  [factor s]
  (->> 
     (let [;s example-input
           image (s/split-lines s)
           gs (find-galaxies image) 
           {exp-cols :cols exp-rows :rows} (find-expanding image)] 
        (for [i (range (count gs))
              j (range (inc i) (count gs))
              :let [[rx cx :as gx] (gs i), [ry cy :as gy] (gs j)
                    n-cexp (count (filter #(< (min cx cy) % (max cy cx))
                                          exp-cols))
                    n-rexp (count (filter #(< (min rx ry) % (max ry rx))
                                          exp-rows))
                    n-exp (+ n-cexp n-rexp)
                    d0 (l1-dist gx gy)
                    d (+ d0 (* (dec (long factor)) n-exp))]]
          d))
     (reduce +)))

(def solution-2 (partial solution 1e6))

(comment 
  (solution-2 (slurp "resources/input/2023/input_11")) ;; 678626199476
  ; Another solution 1b
  (solution 2 (slurp "resources/input/2023/input_11"))) ;; 9556712

