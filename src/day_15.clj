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
  "Returns idx of first empty string on a vector"
  [v]
  (let [size (count v)]
    (loop [idx 0] 
      (if (>= idx size) nil 
        (if (= (v idx) "") idx 
          (recur (inc idx)))))))

(defn parse-input
  "Parses input string into a map containing the map (flattened) moves and width"
  [s]
  (let [;s example-input
        v (s/split-lines s)
        blank (find-empty v)
        vmap (mapv (partial mapv identity) (subvec v 0 blank))
        fmap (reduce into [] vmap)
        moves (reduce into [] (subvec v (inc blank)))]
    {:warehouse {:tiles fmap :width (count (vmap 0))}
     :moves moves})) 

(comment 
  (parse-input example-input))

(defn find-robot
  [tiles]
  (loop [idx 1]
    (if (>= idx (dec (count tiles))) nil
      (if (= \@ (tiles idx)) idx (recur (inc idx))))))

(comment 
  (find-robot (:tiles (:warehouse (parse-input example-input)))))

;; unsure why i chose to make the map flat
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
  "For a give map and position 'scope ahead' in the given direction for a wall or box
   returns the coord until free along with starting or nil"
  [tiles position direction]
  (loop [next (direction position)]
    (case (tiles next)
      \O (recur (direction next))
      \# nil
      \. [next position])))  

(defn make-move 
  "updates map moving the robot in direction"
  [init direction]
  (let [fmap (:tiles (:warehouse init))
        ncols (:width (:warehouse init))
        direction (partial direction ncols)
        robot-position (find-robot fmap)]
     (case (fmap (direction robot-position)) 
           \. (assoc-in init [:warehouse :tiles]
                        (assoc fmap robot-position\. (direction robot-position) \@))
           \# init
           \O (if-let [[free-pos starting] 
                       (scope-direction fmap robot-position direction)]
                 (assoc-in init [:warehouse :tiles]
                           (assoc fmap free-pos \O 
                                       starting \.
                                       (direction starting) \@))
                 init))))

(defn print-map
 [warehouse] 
 (doseq [l (partition (:width warehouse) (:tiles warehouse))]
   (flush)
   (println l)))

(defn print-map-2
 [warehouse] 
 (let [tiles (:tiles warehouse)
       width (:width warehouse)]
   ;(println (apply str (interpose " " (range 0 width))))
   (doseq [n (range 0 (count tiles))]
     ;(print (mod n width))
     (when (= (mod n width) 0) (flush ) (print "\n"))
     (print (tiles n)))))

(comment 
  (print-map-2 (:warehouse (parse-input example-input-2))))
  ;(print-map (:warehouse (reduce make-move (parse-input example-input)
  ;                              (map move-map (:moves (parse-input example-input))))))) 

(defn solution-1
  [s]
  (let [init (parse-input s)
        end (reduce make-move init (map move-map (:moves init)))]
    (reduce +
            (map (fn [[r c]] (+ (* 100 r) c))
                 (map #((partial flat-to-plane (:width (:warehouse init)) (% 0)))
                      (filter #(true? (% 1))
                              (map-indexed (fn [idx v] [idx (= \O v)])
                                           (:tiles (:warehouse end)))))))))

(comment 
  (solution-1 example-input) ;; 2028
  (solution-1 example-input-2) ;; 10092
  (solution-1 (slurp "resources/input/input_15"))) ;; 1552879

;; PART 2
(defn double-char
  "Expands tiles give rules"
  [c]
  (case c
    \# (str \# \#)
    \O (str \[ \])
    \. (str \. \.)
    \@ (str \@ \.)
    (str c)))

(defn expand-map 
  "Apply the doubling to the parsed data"
  [init]
  (let [fmap (:tiles   (:warehouse init))
        width (:width (:warehouse init))]
    (assoc init :warehouse
           {:tiles (into [] (mapcat double-char fmap)) :width (* 2 width)})))

(comment
  (print-map (:warehouse (expand-map (parse-input example-input-2)))))

(defn blocked?
  "Checks if a position is blocked form movement in a direction"
  [warehouse position direction]
  (let [tiles (vec (:tiles warehouse))
        width (:width warehouse)
        npos (direction width position)
        next (tiles npos)]
    (cond 
      (= next \#) true
      (= next \.) false
      (or (= direction left-flat)  
          (= direction right-flat)) (blocked? warehouse (direction width npos) direction)
      (= next \[) (or (blocked? warehouse npos direction)
                      (blocked? warehouse (right-flat width npos) direction))
      (= next \]) (or (blocked? warehouse npos direction)
                      (blocked? warehouse (left-flat width npos) direction)))))

(comment 
  (print-map (:warehouse (expand-map (parse-input example-input-2))))
  (blocked? (:warehouse (expand-map (parse-input example-input-2)))
            130
            up-flat))
            
(defn push 
  "Applies pushing of positions in direction.
  Order in positions matters, must start from edges"
  [warehouse positions direction]
  (let [width (:width warehouse)]
    (loop [to-move positions
           return warehouse]
      (if (empty? to-move) return
        (let [c-pos (peek to-move)
              m-pos (direction width c-pos)
              c ((:tiles return) c-pos)
              r ((:tiles return) m-pos)]
          ;(println {:c-pos c-pos :c c :m-pos m-pos :r r})
          (recur (pop to-move)
                 {:tiles (assoc (:tiles return) m-pos c c-pos r)
                  :width width}))))))
        
(comment 
  (print-map (:warehouse (expand-map (parse-input example-input-2))))
  (print-map (push (:warehouse (expand-map (parse-input example-input-2)))
                   (vector 26 27)
                   right-flat)))

(defn get-push-positions 
  "Gets all the position than have to be pushed once moving in a direction"
  [warehouse position direction]
  (let [tiles (:tiles warehouse)
        width (:width warehouse)]
    (if (blocked? warehouse position direction) ()
      (loop [to-check (list position)
             result ()
             checked #{}]
        (if (empty? to-check) result
          (let [current (first to-check)
                next (direction width current)
                nchar (tiles next)]
            (if (checked current) (recur (rest to-check) result checked) 
              (case nchar
                    \. (recur (rest to-check) (conj result current) (conj checked current))
                    \] (recur (concat (rest to-check) [next (left-flat width next)])
                              (conj result current) (conj checked current))
                    \[ (recur (concat (rest to-check) [next (right-flat width next)])
                              (conj result current) (conj checked current))))))))))

;; expected '(64 65 67 66 85 86 106) for 106 up
(comment 
  (let [fmap (push (:warehouse (expand-map (parse-input example-input-2)))
                  (vector 88 87 86) ;; [88 87 86]
                  left-flat)
        pp (get-push-positions fmap 106 up-flat)]
   (print-map-2 fmap)
   (get-push-positions fmap 106 up-flat)
   (print-map-2 (push fmap pp up-flat))))


(defn move 
  "Applies a move given initial state"
  [init direction]
  (let [wh (:warehouse init)
        tiles (:tiles wh)
        ;width (:width wh)
        r-position (find-robot tiles)
        direction direction
        p-positions (get-push-positions wh r-position direction)]
     (assoc init :warehouse (push wh p-positions direction))))    
    
  
(defn solution-2
  [s]
  (let [;s (slurp "resources/input/input_15")
        init (parse-input s)
        expanded (expand-map init)
        width (:width (:warehouse expanded))
        end (reduce move expanded (map move-map (:moves init)))]
      ;(print-map-2 (:warehouse end))
      (->> end :warehouse :tiles
           (map-indexed (fn [idx v] [idx (= v \[)]))
           (filter #(% 1))
           (map #(flat-to-plane width (% 0)))
           (map (fn [[r c]] (+ (* 100 r) c)))
           (reduce +))))     
            
(comment 
  (solution-2 example-input-2) ;; 9021
  (solution-2 (slurp "resources/input/input_15"))) ;; 1561175

(defn s2 []
  (solution-2 (slurp "resources/input/input_15")))

