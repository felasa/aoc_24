(ns y2023.aoc-10 
  (:require
    [clojure.string :as str]))

(def example-s "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L") 

(def example-input (str/split-lines example-s))

(def puzzle-input
  (str/split-lines (slurp "resources/input/2023/input_10")))

(defn down 
  [[x y]]
  [(inc x) y])
            
(defn up 
  [[x y]]
  [(dec x) y])  

(defn right 
  [[x y]]
  [x (inc y)])  

(defn left
  [[x y]]
  [x (dec y)])

(def char-moves {\| #{:up    :down}
                 \- #{:right :left}
                 \L #{:up    :right}
                 \J #{:up    :left}
                 \7 #{:down  :left}
                 \F #{:down  :right}})

(def kwdir {:up up :down down :right right :left left})

(def anti-dir {up down down up left right right left 
               :up :down :down :up :left :right :right :left})

(defn find-start 
  [input]
  (loop [x 0 y 0] 
    (cond 
      (= \S (get-in input [x y])) [x y]
      (>= x (count input)) nil
      (>= y (count (first input))) (recur (inc x) 0)
      :else (recur x (inc y)))))

(comment 
  (find-start puzzle-input) ;; [38 55]
  (find-start example-input)) ;; [0 4]

(defn infer-start-pipe
  "deduce pipe type from surroundings"
  [data coords]
  (let [possible (transient #{\| \- \L \J \7 \F})]
       ; [u d r l :as dirs] (neighbor-chars coords data)]
    (when (#{\| \7 \F} (get-in data (up coords)))
      (disj! possible \- \7 \F))  
    (when (#{\- \J \7} (get-in data (right coords)))
      (disj! possible \| \J \7))
    (when (#{\| \L \J} (get-in data (down coords)))
      (disj! possible \- \L \J))
    (when (#{\- \L \F} (get-in data (left coords)))
      (disj! possible \| \L \F))
    (first (persistent! possible))))

(defn travel-loop
  [map-data start]
  (loop [position start
         origin-dest nil
         counter 0]
    (if (and (> counter 0) 
             (#{\S} (get-in map-data position)))
      counter
      (let [A (if (not= position start) 
                (get-in map-data position) 
                (infer-start-pipe map-data start))
             
            next-direction (first (disj (char-moves A) 
                                    origin-dest))
            next-position ((next-direction kwdir) position)
            o-dest (anti-dir next-direction)]
        (recur next-position o-dest (inc counter))))))

(defn solution-1 [s]
  (let [map-data (str/split-lines s)
        start (find-start map-data)
        loop-size (travel-loop map-data start)]
    (if (odd? loop-size) (inc (quot loop-size 2)) (quot loop-size 2))))

(comment 
  (solution-1 (slurp "resources/input/2023/input_10"))) ;; 7086

;; PART 2
;; Basically by Jordan Curve theorem (?) we check what direction the 
;;  curve is travelling along relative to a point to determine wether the point is 
;;  inside or outside
(def turn {:up :right :right :down
           :down :left :left :up})

; corners are a problem
(defn travel-loop-2
  "Returns the loop positions and direction of travel"
  [map-data start]
  (loop [position start
         origin-dest nil
         counter 0
         acc {}]
    (if (and (> counter 0) 
             (#{\S} (get-in map-data position)))
      (assoc acc position origin-dest)
      (let [A (if (not= position start) 
                (get-in map-data position) 
                (infer-start-pipe map-data start))
             
            next-direction (first (disj (char-moves A) 
                                        origin-dest))
            o-dir (anti-dir (first (disj (char-moves A) next-direction)))
            next-position ((next-direction kwdir) position)
            o-dest (anti-dir next-direction)]
        (recur next-position o-dest (inc counter)
               (assoc acc position [o-dir next-direction]))))))

(defn h-crossings 
  "For a given row returns the the intersections with (directed) path or wall"
  [path width row]
  (loop [col 0
         travelled []
         return {}]
    (if (>= col width) (into return (map #(vector % :wall) travelled))
      (if-let [intersect (path [row col])]
        (recur (inc col)
               []
               (into return (map #(vector % intersect) travelled)))
        (recur (inc col) (conj travelled [row col]) return)))))
      
(defn v-crossings 
  "For a give column return the intersections with (directed) path or wall"
  [path height col]
  (loop [row 0
         travelled []
         return {}]
    (if (>= row height) (into return (map #(vector % :wall) travelled))
      (if-let [intersect (path [row col])]
        (recur (inc row)
               []
               (into return (map #(vector % intersect) travelled)))
        (recur (inc row) (conj travelled [row col]) return)))))

(defn crossings
  "Merge h and v crossongs"
  [path width height]
  (merge-with vector 
      (reduce into {}
              (map (partial h-crossings path width)
                   (range 0 height)))
      (reduce into {}
              (map (partial v-crossings path height)
                   (range 0 width)))))

(defn take-distinct 
  "get the first n distinct values as a set"
  [n coll]
  (loop [remain coll 
         ret #{}]
    (if (= (count ret) n) ret
      (if-let [this (first remain)]
        (recur (rest remain) (conj ret this))
        ret))))

(defn complete-hpairs
  [pairs]
  (let [option-1 #{[:up :up] [:up :right] [:left :up] :wall}
        option-2 #{[:down :down] [:left :down] [:down :right] :wall}]
    (if (some (disj option-1 :wall) pairs) option-1 option-2))) 

; maybe not all cases will be represented so we complete them
(defn complete-vpairs
  [pairs]
  (let [option-1 #{[:right :right] [:righ :down] [:down :right] :wall}
        option-2 #{[:left :left] [:up :left] [:left :down] :wall}]
    (if (some (disj option-1 :wall) pairs) option-1 option-2))) 

;width and height could be ommited maybe since the interior has to be bounded by path
(defn classify 
  "Return the interior positions of the path"
  [width height path]
  (let [;width 140 height 140 path (travel-loop-2 puzzle-input [38 55])
        crossings (crossings path width height)
        h-outside (->> crossings
                       (filter #(= ((val %) 1) :wall)) 
                       (map #((val %) 0))
                       (take-distinct 4)
                       (complete-hpairs))
        v-outside (->> crossings
                       (filter #(= ((val %) 0) :wall)) 
                       (map #((val %) 1))
                       (take-distinct 4)
                       (complete-vpairs))
        inside    (->> crossings 
                       (remove (or #(h-outside ((val %) 0))
                                   #(v-outside ((val %) 1)))))]
        ;inside (remove (into (set (map first outside))) crossings)]
     inside))   

(comment
  (count (classify 20 10 (travel-loop-2 example-input [0 4]))) ;; 10
  (count (classify 140 140 (travel-loop-2 puzzle-input [38 55])))) ;; 317

(defn solution-2 [s]
  (let [map-data (str/split-lines s)
        H (count map-data)
        W (count (first map-data))
        start (find-start map-data)
        path (travel-loop-2 map-data start)
        inside (classify W H path)]
    (count inside)))

(comment 
  (solution-2 (slurp "resources/input/2023/input_10"))) ;; 317

