(ns bin-day-20
  (:require [clojure.string :as s]))

;; binned previous solution day 20 v. slow, v. verbose
;; other solution is just smarter
(def example-input "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############")

(defn c-at-pos
  [charmap [row col]]
  (get-in charmap [row col] nil))

(defn find-endpoints
  [charmap]
  (let [nrows (count charmap)
        ncols (count (first charmap))]
    (loop [row 0 col 0
           start nil end nil]
      (if (or (>= row nrows) (and start end)) {:start start :end end}
          (if (>= col ncols) (recur (inc row) 0 start end)
              (case (c-at-pos charmap [row col])
                \S (recur row (inc col) [row col] end)
                \E (recur row (inc col) start [row col])
                (recur row (inc col) start end)))))))

(defn neighbors
  [charmap [row col]]
  (filter #(not= (c-at-pos charmap %) \#)
          [[(inc row) col] [(dec row) col] [row (inc col)] [row (dec col)]]))

(defn make-graph
  [charmap]
  (let [nrows (count charmap)
        ncols (count (first charmap))]
    (loop [row 0
           col 0
           graph {}
           start nil
           end nil]
      (if (>= row nrows) {:graph graph :start start :end end}
          (if (>= col ncols) (recur (inc row) 0 graph start end)
              (let [c (c-at-pos charmap [row col])]
                (cond
                  (= c \#) (recur row (inc col) graph start end)
                  (= c \.)
                  (recur row (inc col)
                         (assoc graph [row col]
                                (zipmap (neighbors charmap [row col]) (repeat 1)))
                         start end)
                  (= c \S)
                  (recur row (inc col)
                         (assoc graph [row col]
                                (zipmap (neighbors charmap [row col]) (repeat 1)))
                         [row col] end)
                  (= c \E)
                  (recur row (inc col)
                         (assoc graph [row col]
                                (zipmap (neighbors charmap [row col]) (repeat 1)))
                         start [row col]))))))))

(defn opens-path?
  [charmap [row col]]
  (if (not= \# (c-at-pos charmap [row col])) false
      (let [n-open (->> [[(inc row) col] [(dec row) col]
                         [row (inc col)] [row (dec col)]]
                        (map (partial c-at-pos charmap))
                        (map #(if (and % (not= % \#)) 1 0))
                        (reduce +))]
        (if (>= n-open 2) true false))))

(defn relevant-obstacles
  [charmap]
  (let [nrows (+ -1 (count charmap))
        ncols (+ -1 (count (first charmap)))]
    (loop [row 1 col 1
           ret ()]
      (if (>= row nrows) ret
          (if (>= col ncols) (recur (inc row) 0 ret)
              (if (opens-path? charmap [row col])
                (recur row (inc col) (conj ret [row col]))
                (recur row (inc col) ret)))))))

(defn parse-input
  [s]
  (let [charmap (->> s s/split-lines (mapv vec))
        obstacles (relevant-obstacles charmap)]
    (assoc (make-graph charmap)
           :charmap charmap
           :obstacles obstacles)))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([& args] (into (queue) args)))

(defn shortest-paths
  [graph start]
  (loop [to-visit (queue [start 0])
         return {}]
    ;(Thread/sleep 500) (println {:tv to-visit :v visited :r return})
    (if (empty? to-visit) return
        (let [current (peek to-visit)
              [node distance] current
              neighbors (update-vals (graph node) #(+ distance %))]
          (if-let [pdist (return node)]
            (if (< distance pdist)
              (recur (into (pop to-visit) neighbors)
                     (assoc return node distance))
              (recur (pop to-visit)
                     return))
            (recur (into (pop to-visit) neighbors)
                   (assoc return node distance)))))))

(comment 
  (let [data (parse-input example-input)
        graph (:graph data)
        start (:start data)
        end (:end data)]
   (shortest-paths graph [end 0])))

(defn remove-obstacle
  [graph [row col]]
  (let [neighbor-positions [[(inc row) col] [(dec row) col]
                            [row (inc col)] [row (dec col)]]
        affected (filter #(graph %) neighbor-positions)]

    (reduce (fn [l r] (assoc-in l [r [row col]] 1))
            (assoc graph [row col] (zipmap affected (repeat 1)))
            affected)))

(comment
  (parse-input example-input))

(defn try-cheats
  [data]
  (let [graph (:graph data)
        start (:start data)
        end (:end data)]
   ;(println end)
   ;((shortest-paths (:graph data) (:start data)) end) 
    (loop [to-check (:obstacles data)
           return [((shortest-paths (:graph data) (:start data)) end)]]
     ;(println :tc to-check :r return)
      (if (empty? to-check) return
          (let [obstacle (first to-check)
                new-graph (remove-obstacle graph obstacle)
                dist ((shortest-paths new-graph start) end)]
            (recur (rest to-check) (conj return dist)))))))

(defn try-cheats-m
  [data]
  (let [graph (:graph data)
        start (:start data)
        end (:end data)
        obstacles (:obstacles data)]
    (conj (pmap 
            (fn [coord] 
              ((shortest-paths (remove-obstacle graph coord) start)
               end obstacles)))
          ((shortest-paths graph start) end))))

(defn solution-1 [s] 
  (let [;data (parse-input example-input)
        data (parse-input s)
        distances (try-cheats data)]
    (reduce +
            (map #(if (>= (- (first distances) %) 100) 1 0)
                 distances))))

(defn solution-1b [s]
  (let [;data (parse-input example-input)
        data (parse-input s)
        distances (try-cheats-m data)]
    (reduce +
            (map #(if (>= (- (first distances) %) 100) 1 0)
                 distances))))

(comment
  (time (solution-1  (slurp "resources/input/input_20")))   ;; 1393
  (time (solution-1b (slurp "resources/input/input_20"))))  ;; 1393
