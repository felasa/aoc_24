(ns day-16
  (:require [clojure.string :as string]))

(def example-input "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")

(def example-input-2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################")

(defn find-endpoints
  "Return the start and end coordinates"
  [maze]
  (let [nrows (count maze)
        ncols (count (first maze))]
    (loop [row 0 col 0
           start nil end nil]
      (if (and start end) [start end]
          (if (>= row nrows) nil
              (if (>= col ncols) (recur (inc row) 0 start end)
                  (let [c (get-in maze [row col])]
                    (case c
                      \S (recur row (inc col) [row col] end)
                      \E (recur row (inc col) start [row col])
                      (recur row (inc col) start end)))))))))

(defn parse-input
  [s]
  (let [maze (->> s string/split-lines (mapv vec))
        [start end] (find-endpoints maze)]
    {:maze maze :start start :end end}))

;; just use get-in
(defn c-at-position
  [maze coord]
  (get-in maze coord nil))

(comment
  (let [data (parse-input example-input)
        maze (:maze data)]
    (c-at-position maze [1 13])
    (reduce get maze [1 13])
    (get-in maze [1 13]))
  (parse-input example-input)
  (parse-input example-input-2)
  (parse-input (slurp "resources/input/2024/input_16")))

(defn east [[row col]] [row (inc col)])
(defn west [[row col]] [row (dec col)])
(defn north [[row col]] [(dec row) col])
(defn south [[row col]] [(inc row) col])

(def direction-map
  {"E" east "W" west "N" north "S" south})

(defn opposite
  [dir]
  (case dir
    "E" "W"
    "W" "E"
    "N" "S"
    "S" "N"))

;slow, can we skip some more paths?
;; remove turns that lead nowhere since 180 turns means going back
;;   (and start is in corner)
(defn neighbors-n-distance
  ([maze [coord direction]] (neighbors-n-distance maze [coord direction] 0))
  ([maze [coord direction] distance]
   (case direction
     "E" (remove #(= (get-in maze (get-in % [0 0]) \#) \#)
                 (list (vector [((direction-map "E") coord) "E"] (+ distance 1))
                       (vector [coord "S"] (+ distance 1000))
                       (vector [coord "N"] (+ distance 1000))))
     "S" (remove #(= (get-in maze (get-in % [0 0]) \#) \#)
                 (list (vector [((direction-map "S") coord) "S"] (+ distance 1))
                       (vector [coord "W"] (+ distance 1000))
                       (vector [coord "E"] (+ distance 1000))))
     "W" (remove #(= (get-in maze (get-in % [0 0]) \#) \#)
                 (list (vector [((direction-map "W") coord) "W"] (+ distance 1))
                       (vector [coord "N"] (+ distance 1000))
                       (vector [coord "S"] (+ distance 1000))))
     "N" (remove #(= (get-in maze (get-in % [0 0]) \#) \#)
                 (list (vector [((direction-map "N") coord) "N"] (+ distance 1))
                       (vector [coord "E"] (+ distance 1000))
                       (vector [coord "W"] (+ distance 1000)))))))

(comment
  (let [data (parse-input example-input)
        ;data (parse-input (slurp "resources/input/2024/input_16"))
        maze (:maze data)
        coord [9 1]
        dir "E"
        distance 0]
    (neighbors-n-distance maze [coord dir] 0)))
    ;,(doseq [l maze] (println l))))

;slow
; got faster by adding opposite direcion to visited nodes, not enough imo
(defn shortest-path
  [maze [[start-row start-col] start-direction]]
  (loop [to-visit (list (vector [[start-row start-col] start-direction] 0))
         visited {}]
    ;(Thread/sleep 1000) (println {:v visited});:tv (map #(% 0) to-visit)})
    (if (empty? to-visit) visited
        (let [[[coord dir] dist] (peek to-visit)
              neighbors (neighbors-n-distance maze [coord dir] dist)]
          (if-let [prev-dist (get visited [coord dir])]
            (if (< dist prev-dist)
              (recur (into (pop to-visit) neighbors)
                     (assoc visited [coord dir] dist)) 
              (recur (rest to-visit) visited))
            (recur (into (pop to-visit) neighbors)
                   (assoc visited [coord dir] dist [coord (opposite dir)]
                          (+ dist 2000))))))))

(defn simple-neighbors
  [maze coord]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map (fn [[row col]] [(+ row (coord 0)) (+ col (coord 1))]))
       (remove #(= (get-in maze %) \#))))       

(comment
  (let [data (parse-input example-input)
        ;data (parse-input (slurp "resources/input/2024/input_16"))
        maze (:maze data)]
    (simple-neighbors maze [13 1])))
    
(defn paths-to-end 
  [maze start end]
  (loop [partial-paths [(list start)]
         paths []
         visited #{}]
    (if (empty? partial-paths) paths
      (let [current-path (peek partial-paths)
            coord (peek current-path)
            neighbors (simple-neighbors maze coord)]
        (if (= coord end)
          (recur (pop partial-paths)
                 (conj paths current-path)
                 visited)
          (if (visited coord) (recur (pop partial-paths) paths visited)
            (recur (into (pop partial-paths) (map #(conj current-path %) neighbors))
                   paths
                   (conj visited coord))))))))
        
(comment
  (let [data (parse-input example-input)
        ;data (parse-input (slurp "resources/input/2024/input_16"))
        maze (:maze data)
        start (:start data) end (:end data)]
    (paths-to-end maze start end)))

(comment
  (let [s example-input
        data (parse-input s)
        maze (:maze data)
        start (:start data)]
    (shortest-path maze [start "E"]))
  (into [] (range 0 3))) 

(defn solution-1
  [s]
  (let [;s example-input
        ;s (slurp "resources/input/2024/input_16")
        data (parse-input s)
        maze (:maze data)
        start (:start data)
        end (:end data)]
    (->> (shortest-path maze [start "E"])
         (filter #(= (get (key %) 0) end))
         (map #(% 1))
         (reduce min))))

(comment
  (solution-1 example-input) ;; 7036
  (solution-1 example-input-2) ;; 11048
  ;about 1.7minutes
  (time (solution-1 (slurp "resources/input/2024/input_16")))) ;; 98416

;; PART 2
; Mostly stolen from 
; https://github.com/erdos/advent-of-code/blob/master/2024/day16.clj
; removed dependency from loom and implemented algos from scratch to understand them

; TIL that clojure has queues
(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([& args] (into (queue) args))) 

(defn maze-graph
  "Creates a map representing a directed graph in the form {node {neighbor weight/distance}} from the maze. nieghbors is a function that given a coordinate returns the 
  neighboring positions and distance"
  [maze neighbors]
  (let [nrows (count maze)
        ncols (count (first maze))
        dirs ["E" "W" "S" "N"]]
    (loop [row 1 col 1
           d-idx 0
           G {}] ;[east west south north]]
      (if (>= row (dec nrows)) G
          (if (>= col (dec ncols)) (recur (inc row) 0 0 G)
              (if (>= d-idx 4) (recur row (inc col) 0 G)
                  (let [d (dirs d-idx)
                        N (neighbors maze [[row col] d])
                        subG (reduce #(apply assoc %1 %2) {} N)]
                    (if (seq N)
                      (recur row col (inc d-idx) (assoc G [[row col] d] subG))
                      (recur row col (inc d-idx) G)))))))))

;; tryng graph algo
;;; faster paritcularly with queues
(defn trvrse
  "shortest distances"
  [graph start]
  (loop [queue (queue [start 0])
         visited {}]
    (if (empty? queue) visited 
      (let [[current dist] (peek queue)
            neighbors-n-dist (remove #(>= (val %) (get visited (key %) ##Inf))
                                     (update-vals (graph current) #(+ dist %)))]
        (recur (into (pop queue) neighbors-n-dist) 
               (assoc visited current dist))))))

(defn solution-1b
  [s]
  (let [;s example-input
        ;s (slurp "resources/input/2024/input_16")
        data (parse-input s)
        maze (:maze data)
        start (:start data)
        end (:end data)
        graph (maze-graph maze neighbors-n-distance)]
    (->> (trvrse graph [start "E"])
         (filter #(= (get (key %) 0) end))
         (map #(% 1))
         (reduce min))))

(comment
  (solution-1b example-input-2) ;; 11048
  ;about 9seconds
  (time (solution-1b (slurp "resources/input/2024/input_16")))) ;; 98416

;;; aight this is the strat:
;; hack the graph so end coord without direction is a node (0 distance? nope. 
;;   just replace
;; do shortest distances but add the predecessor
;, reverse graph, do shortest distances
;; find middle ground nodes
;; ???
;;profit

; collapse end node into a single thing
(defn no-turn-end 
  "Changes [end, dir] destination nodes in graph to just end"
  [graph end]
  (update-vals graph
    (fn [nbs] (update-keys nbs #(if (= (% 0) end) end %)))))

(defn reverse-graph 
  "Reverses graph directions"
  [graph]
  (let [entries (for [[o nbs] graph [nb dist] nbs] [nb o dist])]
    (reduce (fn [m [nb o dist]] (assoc-in m [nb o] dist))
            {}
            entries)))
    
(comment
  (reverse-graph 
    {:a {:b 1 :c 3} :z {:x 400 :a 3 :b 50}
     :w {:b 3 :a 12 :z 30 :x 38}})
  (let [G {:a {:b 1 :c 3} :z {:x 400 :a 3 :b 50}
           :w {:b 3 :a 12 :z 30 :x 38}}]
    (= G (reverse-graph (reverse-graph G))))
  (no-turn-end {:a {:b 6 [:z "e"] 9}} :z)
  (no-turn-end {[[1 1] "E"] {[[6 5] "W"] 6 [[1 13] "e"] 9}} [1 13]))

; visited {node [distance pred], ...}
; nbs {node dist, ...}
(defn sps
  "shortest distances with predecessor"
  [graph start]
  (loop [queue (queue [start [nil 0]])
         visited {start [nil 0]}]
    (if (empty? queue) (update-vals visited #(apply hash-map %)) 
      (let [[current [prr dist]] (peek queue)
            neighbors-n-dist (map #(vector (% 0) [current (% 1)])
                                  (remove #(> (val %)
                                              (get-in visited [(key %) 1] ##Inf))
                                          (update-vals (graph current) #(+ dist %))))]
        (recur (into (pop queue) neighbors-n-dist) 
               (into visited neighbors-n-dist))))))

(defn sps-span
  "span like in loom. shortest distances from start but structured as an 
  adjacency graph"
  [graph start]
  ;TODO: implement directly?
  (reverse-graph (sps graph start)))

(defn solution-2 [s] 
  (->> 
    (let [
          data   (parse-input s)
          maze   (:maze data)
          start  (:start data)
          end    (:end data)
          graph  (no-turn-end (maze-graph maze neighbors-n-distance) end)
          rgraph (reverse-graph graph) 
          g-span (sps-span graph [start "E"])
          best-d (reduce min (remove nil? (map (fn [n] ((val n) end)) g-span)))
          rg-span (sps-span rgraph end)]
      (for [node (keys graph)
            [succ dist1] (g-span node)
            [pred dist2] (rg-span node)
            ;account for pred and succ be contiguos
            :let [df (- best-d (+ dist1 dist2))]
            :when (and (#{-2 -2000 -1001} (- best-d (+ dist1 dist2))))]
        node))
    (map #(% 0))
    (distinct)
    (count)
    (inc)))

(comment
  (solution-2 example-input) ;; 45
  (solution-2 example-input-2) ;; 64
  (solution-2 (slurp "resources/input/2024/input_16"))) ;; 471

;; some things i tried that didn't pan out but maybe because faulty implementation
;; further along the line
;; - when creating neighbors skip the turn step or remove turns that face walls
;; - add end node without dir with distance 0
;; - walk the graph and return all paths
