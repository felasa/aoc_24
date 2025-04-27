(ns y2023.day-17
  (:require [clojure.string :as s]
            [util.util :refer [queue]]
            [clojure.data.priority-map :refer [priority-map]]))

(def example-input "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defn parse-input [s]
  (mapv (partial mapv #(parse-long (str %)))
        (s/split-lines s)))

(def example-data
  (parse-input example-input))

(def directions [[0 1] [1 0] [-1 0] [0 -1]])

(defn move
  "Moves coordinate in direction given. return new state"
  [cities dir [coord heat direction consecutive]]
  (let [newpos (mapv + coord dir)
        newheat (+ heat (get-in cities newpos ##Inf))
        newcount (if (= direction dir) (inc consecutive) 1)]
    [newpos newheat dir newcount]))

;; Initially i tried solving this a simple queue that just move one step at a time
;;  while enforcing the consecutive rule. This didnt work since we couldn't accurately
;;  keep track of optimal paths, and when we did it was slooow. All solutions I saw looked ahead 
;; to populate the possible the queue with better options first.
;; References:
;; (without priority map!)
;; https://github.com/abyala/advent-2023-clojure/blob/main/src/advent_2023_clojure/day17.clj 
;; https://github.com/narimiran/AdventOfCode2023/blob/main/clojure/day17.clj

(defn all-straight-moves
  "Returns all possible consecutive moves in a direction from state given min and max times"
  [cities state mintimes maxtimes dir]
  (->> state
       (iterate (partial move cities dir))
       (take (inc maxtimes))
       (drop mintimes)
       (remove #(> (% 3) maxtimes))))

(comment
  (all-straight-moves example-data [[5 5] 5 [0 1] 2] 1 3 [1 0]))
;; ([[6 5] 12 [1 0] 1] [[7 5] 19 [1 0] 2] [[8 5] 25 [1 0] 3])

(defn prio-fn
  "Returns a priority for a next move based on distance to end and current cost (heat)"
  [end [npos nheat _ _]]
  (+ (reduce + (mapv - end npos))
     nheat))

(defn next-moves-weighted
  "next moves with priority
   1. Moving towards end. 
   2. Prefer consecutive moves. 3. ??heat???"
  [cities [cpos cheat cdir ccons]]
  (let [end [(dec (count (first cities))) (dec (count cities))]]
    (->> directions
         (remove #(= (mapv + cdir %) [0 0]))
         ;(map (move-fn cities [cpos cheat cdir ccons]))
         (mapcat (partial all-straight-moves cities [cpos cheat cdir ccons] 1 3))
         ;(remove nil?)
         (map #(vector % ((partial prio-fn end) %))))))

(comment
  (next-moves-weighted example-data [[5 5] 6 [1 0] 3])
;; ([[[5 6] 14 [0 1] 1] 27]
;;  [[[5 7] 21 [0 1] 2] 33]
;;  [[[5 8] 30 [0 1] 3] 41]
;;  [[[5 4] 11 [0 -1] 1] 26]
;;  [[[5 3] 19 [0 -1] 2] 35]
;;  [[[5 2] 22 [0 -1] 3] 39])
  (next-moves-weighted example-data [[5 5] 6 [1 0] 2]))
;; ([[[5 6] 14 [0 1] 1] 27]
;;  [[[5 7] 21 [0 1] 2] 33]
;;  [[[5 8] 30 [0 1] 3] 41]
;;  [[[6 5] 13 [1 0] 3] 26]
;;  [[[5 4] 11 [0 -1] 1] 26]
;;  [[[5 3] 19 [0 -1] 2] 35]
;;  [[[5 2] 22 [0 -1] 3] 39])

(defn travel
  [cities]
  (let [H (dec (count cities)) W (dec (count (first cities)))]
    (loop [Q1 (priority-map [[0 0] 0 nil 0] 0)
           visited {}]
      (let [[[node vheat dir _ :as visiting] _] (peek Q1)]
        (if (= node [H W])
          vheat
          (if (get visited [node dir])
            (recur (pop Q1) visited)
            (let [nns (next-moves-weighted cities visiting)]
              (recur (into (pop Q1) nns)
                     (assoc visited [node dir] vheat)))))))))

(defn solution-1 [s]
  (->> s parse-input travel))

(comment 
  (solution-1 example-input) ;; 102
  (solution-1 (slurp "resources/input/2023/input_17"))) ;; 694

