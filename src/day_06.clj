(ns day-06
  (:require [clojure.string :as s]))

(def example-input
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...") 

(defn parse-input
  [s]
  (-> s s/split-lines)) 

(defn get-chat-at-pos
  [map row col]
  (if-let [line (get map row)]
    (get line col)))

(defn find-start
  [map]
  (loop [row 0]
    (if (>= row (count map)) nil
      (let [line (get map row)
            matcher (re-matcher #"<|>|v|\^" line)]
        (if (. matcher find)
          [row (. matcher start)]
          (recur (inc row)))))))

(defn direction-vector 
  [char]
  (case char
    \< [0 -1]
    \> [0  1]
    \^ [-1 0]
    \v [1  0]))

(defn turn [direction]
  (case direction
    [-1 0] [0  1]
    [0  1] [1  0]
    [1  0] [0 -1]
    [0 -1] [-1 0]))
    
(defn travel 
  [map guard-position direction]
  (loop [visited #{[guard-position, direction]}
         [row col] guard-position
         direction direction]
    (if (get-chat-at-pos map row col)
      (let [npos [(+ row (direction 0)) (+ col (direction 1))] 
            next (get-chat-at-pos map (npos 0) (npos 1))]
        (case next
          nil visited
          \# (recur (conj visited [[row col] (turn direction)])
                    [row col]
                    (turn direction))
          (recur (conj visited [npos direction]) npos direction)))
      visited)))  

(defn solution-1 
  [s]
  (let [data (parse-input s)
        start (find-start data)
        grd (apply (partial get-chat-at-pos data) start)
        d-vector (direction-vector grd)]
    (->> (travel data start d-vector)
         (map #(get % 0))
         set
         count)))        
(comment 
  (solution-1 example-input)) ;; 41

(defn s1 []
  (solution-1 (slurp "resources/input/input_06")))

(s1) ;; 4696
