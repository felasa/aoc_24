(ns day-20
  (:require [clojure.string :as s]))

;; reworked solutions 
;; Idea lifted from https://narimiran.github.io/aoc2024/clojure/day20/ but implementation is my own
;; the idea is to look for positions within an l1-radius of cheat picoseconds 
;;   instead of trying to walk a graph 
;; much more compact, simpler, faster and leverages the fact that there's only one path towards end

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

(defn find-endpoints
  "find the coordinates of S and E on the map represented as a vector of vectors of chars"
  [charmap]
  (let [nrows (count charmap)
        ncols (count (first charmap))]
    (loop [row 0 col 0
           start nil end nil]
      (if (or (>= row nrows) (and start end)) {:start start :end end}
          (if (>= col ncols) (recur (inc row) 0 start end)
              (case (get-in charmap [row col] nil)
                \S (recur row (inc col) [row col] end)
                \E (recur row (inc col) start [row col])
                (recur row (inc col) start end)))))))

(comment 
  (find-endpoints 
    (mapv vec (s/split-lines example-input)))) ;; {:start [3 1], :end [7 5]}

(defn path-to-end
  "Construct the path towards endpoint from the map. Assumes unique path"
  ([charmap from] (path-to-end charmap #{} from))
  ([charmap visited from]
   (if (nil? from) nil
     (let [next (->> [[1 0] [-1 0] [0 1] [0 -1]]
                    (map #(mapv + from %))
                    (remove #(visited %))
                    (remove #(= \# (get-in charmap %)))
                    first)]
       (lazy-seq 
         (cons from (path-to-end charmap (conj visited from) next)))))))

(defn make-moves
  "delta vectors for neighbors within cheatseconds l1-radius"
  [cheatseconds]
  (for [row-delta (range (- cheatseconds) (inc cheatseconds))
        col-delta (range (- cheatseconds) (inc cheatseconds))
        :when (<= (+ (abs row-delta) (abs col-delta)) cheatseconds)]
    [row-delta col-delta]))

(defn dist-l1
  "l1 (manhattan) distance"
  [x y]
  (reduce + (map (comp abs -) x y)))
        
(defn cheats
  "Return a list of all the possible cheats in path of cheatsize picoseconds
  that save at least target picoseconds"
  [path cheatsize target]
  ;for purity i'd like to replace this for comprehension
  (for [coord (keys path)
        move (make-moves cheatsize)
        :let [d (path coord) 
              cheated (mapv + coord move)
              dch (path cheated)]
        :when (and (path cheated) (>= (- d dch (dist-l1 coord cheated)) target))]
    [coord cheated (- d dch 2)]))

(defn solution-1 [s]
  (let [;s example-input
        charmap (->> s s/split-lines (mapv vec))
        {end :end} (find-endpoints charmap)
        path (into {} (map vector (path-to-end charmap end) (range)))]
    (count (cheats path 2 100))))

;lags a bit, can it be faster?
(defn solution-2 [s]
  (let [;s example-input
        charmap (->> s s/split-lines (mapv vec))
        {end :end} (find-endpoints charmap)
        path (into {} (map vector (path-to-end charmap end) (range)))]
    (count (cheats path 20 100))))

(comment 
  (solution-1 (slurp "resources/input/input_20")) ;; 1393
  (solution-2 (slurp "resources/input/input_20"))) ;; 990096
