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
  (solution-1 example-input) ;; 41
  (solution-1 (slurp "resources/input/input_06"))) ;; 4696

(defn s1 []
  (solution-1 (slurp "resources/input/input_06")))

;; BRUTE FORCE PART 2
(defn loops? 
  "detects loops"
  [map guard-position direction]
  (loop [visited #{[guard-position, direction]}
         [row col] guard-position
         direction direction]
    (if (get-chat-at-pos map row col)
      (let [npos [(+ row (direction 0)) (+ col (direction 1))] 
            next (get-chat-at-pos map (npos 0) (npos 1))]
        (cond 
          (nil? next) 0
          (visited [npos direction]) 1
          (= next \#) (recur (conj visited [[row col] (turn direction)])
                             [row col]
                             (turn direction))
          :else (recur (conj visited [npos direction]) npos direction)))
      0)))  

(let [map-data (parse-input example-input)
      guard-position (find-start map-data)
      guard (apply (partial get-chat-at-pos map-data) guard-position)
      direction (direction-vector guard)]
  (loops? map-data guard-position direction))

(defn add-obstacle
  [map-data [row col]]
  (let [row-s (map-data row)]
    (assoc map-data row (str (subs row-s 0 col) \# (subs row-s (inc col))))))

(defn free-positions 
  [map-data]
  (loop [row 0 col 0 ret ()]
    (if (>= row (count map-data)) ret
      (if (>= col (count (first map-data))) (recur (inc row) 0 ret)
        (if (= \. (get-chat-at-pos map-data row col))
          (recur row (inc col) (conj ret [row col]))
          (recur row (inc col) ret))))))       

(defn solution-2 [s]
  (let [map-data (parse-input s)
        guard-position (find-start map-data)
        guard (apply (partial get-chat-at-pos map-data) guard-position)
        direction (direction-vector guard)
        free-positions (free-positions map-data)]
    (->> free-positions 
         ;look into reducers lib
         (pmap (fn [[row col]] (loops? (add-obstacle map-data [row col])
                                       guard-position direction)))
         (reduce +))))

(comment 
  (solution-2 example-input) ;; 6
  (solution-2 (slurp "resources/input/input_06"))) ;; 1443

(defn s2 []
  (solution-2 (slurp "resources/input/input_06")))

;; PART 2 idea
;; Can deduce from travel history??:
;;  - must exist a repeat partial position (only for direct divertion into loop)
;;  - second arrival has to be from the inverse (counter-clockwise) turn 
;;  - that guarantees a loop if we place an obstacle that forces a turn
;; place before if traveling  left below any spot that has an up path
;; place after  if travelling right above any spot that has a down path
;; place above  if travelling  down to the left of any spot that contains a right path
;; place under  if travlling up to the right of any sport that contains a left path
;; ... Not exhaustive tho, you can place anywhere else where a turn makes it 
;;   take a same path so 
;; obstacle could also be placed so path is directed into another obstacle that directs into loop
;; latter part not looked into
;; could obstacle for a loop in a new path? probably too

(defn travel2 
  "Like travel but for each visited position stores the step number and direction"
  [map guard-position direction]
  (loop [step 0
         visited (list {:step step :coord guard-position :direction direction :corner? true})
         [row col] guard-position
         direction direction]
    (if (get-chat-at-pos map row col)
      (let [npos [(+ row (direction 0)) (+ col (direction 1))] 
            next (get-chat-at-pos map (npos 0) (npos 1))]
        (case next
          nil visited
          \# (recur (inc step)
                    (conj visited {:step (inc step) :coord [row col] 
                                   :direction (turn direction) :corner? true})
                    [row col]
                    (turn direction))
          (recur (inc step) 
                 (conj visited {:step (inc step) :coord npos
                                :direction direction :corner? false})
                 ;(assoc visited npos (assoc (visited npos) step direction))
                 npos direction)))
      visited)))  

(defn solution-1b 
  [s]
  (let [data (parse-input s)
        start (find-start data)
        guard-state (apply (partial get-chat-at-pos data) start)
        d-vector (direction-vector guard-state)]
     (count (distinct (map :coord (travel2 data start d-vector))))))  

(comment 
  (solution-1b example-input) ;; 41
  (solution-1b (slurp "resources/input/input_06"))) ;; 4696

(let [path (travel2 (parse-input example-input) [6 4] [-1 0])]
  (filter :corner? path))

(def d-map {[-1  0] (fn [v] (apply max-key #((:coord %) 0) v))
            [ 1  0] (fn [v] (apply min-key #((:coord %) 0) v))
            [ 0 -1] (fn [v] (apply max-key #((:coord %) 1) v))
            [ 0  1] (fn [v] (apply min-key #((:coord %) 1) v))})

(defn corners-by-direction
  [path direction]
  {direction (update-vals 
               (group-by #((:coord %) 1) 
                         (filter #(= (:direction %) direction) path))
               (d-map direction))})
(defn corners 
  [path]
  (reduce (fn [m, d] (conj m ((partial corners-by-direction path) d))) {}
    [[-1 0] [1 0] [0 -1] [0 1]]))

(comment 
 (corners (travel2 (parse-input example-input) [6 4] [-1 0]))
 (filter :corner? (travel2 (parse-input example-input) [6 4] [-1 0])))

(defn cclock 
  "rotate 90 counter clockwise"
  [v]
  (vector (- (get v 1)) (get v 0)))

(defn obstacle-between?
  [map coord1 coord2]
  (if (= (coord1 0) (coord2 0)) 
    (let [start (min (coord1 1) (coord2 1))
          end (max (coord1 1) (coord2 1))]
      (not-every? #(not= \# %)
                  (for [pos (range (inc start) end)]
                     (get-chat-at-pos map (coord1 0) pos)))) 
    (let [start (min (coord1 0) (coord2 0))
          end   (max (coord1 0) (coord2 0))]
      (not-every? #(not= \# %)
                  (for [pos (range (inc start) end)]
                     (get-chat-at-pos map pos (coord1 1)))))))

(defn obstacles-for-loop
  "Returns coords to place an obstacle to create a loop into provided step"
  [data path step]; {:step 0, :coord [6 4], :direction [-1 0]}]
  (->> (filter #(and (> (:step %) (:step step))
                     (= (:direction %) (cclock (:direction step)))
                     (if (= ((:direction step) 0) 0)
                         (= ((:coord %) 0) ((:coord step) 0))
                         (= ((:coord %) 1) ((:coord step) 1)))
                     ((complement obstacle-between?) data (:coord step) (:coord %)))
                path)
       ;check for obstacles here
       (map #(mapv + (:coord %) (:direction %)))))

(comment 
  (let [s (slurp "resources/input/input_06")
        data (parse-input s)
        start (find-start data) 
        path (travel2 data start [-1 0])]
     (count (distinct (reduce concat (map (partial obstacles-for-loop path) (filter :corner? path))))))) 

;not corrrect
(defn solution-2b 
  [s]
  (let [data (parse-input s)
        start (find-start data)
        guard-state (apply (partial get-chat-at-pos data) start)
        d-vector (direction-vector guard-state)
        path (travel2 data start d-vector)
        corners (filter :corner? path)]
    (->> corners
         ;vals
         ;(reduce concat)
         ;(map val)
         (map (partial obstacles-for-loop data path))
         (reduce concat)
         (distinct))))
         ;count)))
  
(comment 
  (solution-2 example-input)  ;; 6
  (solution-2 (slurp "resources/input/input_06")));; 735 X doesnt account for deviations in between
;; 340
