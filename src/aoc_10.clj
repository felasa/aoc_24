(ns aoc-10 
  (:require
    [clojure.string :as str]))


(def puzzle-input  (str/split-lines (slurp "input_10")))
(type puzzle-input)
(get (puzzle-input 0) 0)
(get "abc" 0)
(defn get-char 
  [coords input]
  (let [[x y] coords]
    (get (get input x) y)))

(get-char [0 1] puzzle-input)

(mapv identity "abc")

(defn down 
  [coords]
  (let [[x y] coords]
    [x (inc y)]))
            
(defn up 
  [coords]
  (let [[x y] coords]
    [x (dec y)]))  

(defn right 
  [coords]
  (let [[x y] coords]
    [(inc x) y]))  

(defn left
  [coords]
  (let [[x y] coords]
    [(dec x) y]))

(def char-moves {\| #{up    down}
                 \- #{right left}
                 \L #{up    right}
                 \J #{up    left}
                 \7 #{down  left}
                 \F #{down  right}})

(defn find-start 
  [input]
  (loop [x 0
         ;y 0 
         lines input]
    (if (empty? lines) 
      nil
      (let [idx (str/index-of (get input x) \S)]
        (if idx
          [x idx]
          (recur (inc x) (rest input)))))))
 
(comment 
  (find-start puzzle-input)
  (get-char [38 55] puzzle-input))

(defn neighbors-coords 
  [coords]
  (map #(% coords) [up down right left]))

(defn neighbor-chars 
  [coords map-data]
  (map #(get-char % map-data) (neighbors-coords coords)))
  
(comment 
  (neighbors-coords [38 55]) ;; ([38 54] [38 56] [39 55] [37 55])
  (neighbor-chars [38 55] puzzle-input)) ;; (\7 \L \| \|)

(defn deduce-destinations-from-coord
  "Assumes it's valid" 
  [coords data]
  (let [possible #{\| \- \L \J \7 \F}
        [u d r l] (neighbor-chars coords data)]
    (if (= u \| or \7 or \F))))
  ;;check if up    is one of \|, \7, \F => | J L 
  ;;check if right is one of \-, \J, \7 => - L F
  ;;check if down  is one of \|, \L, \J => | 7 F 
  ;;check if left  is one of \-, \L, \F => - 7 J
    

(defn travel-forward
  [coords origin map-data]
  (let [moves (char-moves (get-char coords map-data))
        next-direction (first (disj moves origin))]
    (next-direction coords)))

(first #{\a})
(get-char [38 56] puzzle-input) ;; \L
(travel-forward [38 56] right puzzle-input) ;; [38 55]
(travel-forward [38 56] up puzzle-input) ;; [39 56]
(first (disj (char-moves (get-char [38 56] puzzle-input)) right)) ;; \L
          
        
