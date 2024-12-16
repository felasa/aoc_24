(ns day-15
  (:require [clojure.string :as s]))

(def example-input "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(def example-input-2 "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(defn find-empty 
  [v] 
  (let [size (count v)]
    (loop [idx 0] 
      (if (>= idx size) nil 
        (if (= (v idx) "") idx 
          (recur (inc idx)))))))

(defn parse-input [s]
  (let [;s example-input
        v (s/split-lines s)
        blank (find-empty v)
        map (mapv (partial mapv identity) (subvec v 0 blank))
        fmap (reduce into [] map)
        moves (reduce into [] (subvec v (inc blank)))]
    {:map fmap :moves moves :ncols (count (map 0))})) 

(comment 
  (parse-input example-input)
  (reduce into (:map (parse-input example-input))))

(defn find-robot
  [map]
  (loop [idx 1]
    (if (>= idx (dec (count map))) nil
      (if (= \@ (map idx)) idx (recur (inc idx))))))

(comment 
  (find-robot (:map (parse-input example-input))))

(defn flat-to-plane [ncols n]
  [(quot n ncols) (rem n ncols)])

(defn plane-to-flat [ncols [row col]]
  (+ (* row ncols) col))

(defn up-flat [ncols idx]
  (- idx ncols))

(defn down-flat [ncols idx]
  (+ idx ncols))

(defn right-flat [_ idx]
  (+ idx 1))

(defn left-flat [_ idx]
  (- idx 1))

(def move-map {\^ up-flat \v down-flat
               \> right-flat \< left-flat})
            
(comment 
  (map (partial flat-to-plane 3) (range 0 12))
  (map (partial plane-to-flat 3) (for [row (range 0 4) col (range 0 3)] [row col])))

(defn scope-direction 
  [map position direction]
  (loop [next (direction position)]
    (case (map next)
      \O (recur (direction next))
      \# nil
      \. [next position])))  

(defn make-move 
  [init direction]
  (let [flat-map (:map init)
        ncols (:ncols init)
        direction (partial direction ncols)
        robot-position (find-robot (:map init))
        fmap flat-map]
     (case (fmap (direction robot-position)) 
           \. (assoc-in init [:map]
                        (assoc fmap robot-position\. (direction robot-position) \@))
           \# init
           \O (if-let [[free-pos starting] 
                       (scope-direction fmap robot-position direction)]
                 (assoc-in init [:map]
                           (assoc fmap free-pos \O 
                                       starting \.
                                       (direction starting) \@))
                 init))))

(defn print-map
 [map] 
 (doseq [l (partition 8 map)]
    (println l)))

(comment 
  (print-map (:map (reduce make-move (parse-input example-input)
                            (map move-map (:moves (parse-input example-input))))))) 

(defn solution-1
  [s]
  (let [init (parse-input s)
        end (reduce make-move init (map move-map (:moves init)))]
    (reduce +
            (map (fn [[r c]] (+ (* 100 r) c))
                 (map #((partial flat-to-plane (:ncols init)) (% 0))
                      (filter #(true? (% 1))
                              (map-indexed (fn [idx v] [idx (= \O v)])
                                           (:map end))))))))

(comment 
  (solution-1 example-input) ;; 2028
  (solution-1 example-input-2) ;; 10092
  (solution-1 (slurp "resources/input/input_15"))) ;; 1552879

