(ns y2023.day-18
  (:require [clojure.string :as s]))

(def example-input "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(defn parse-input
  "Split into vectors parsing the int"
  [s]
  (->> s s/split-lines
       (mapv #(s/split % #" "))
       (mapv #(assoc % 1 (parse-long (% 1))))))

(comment
  (parse-input example-input))

(def dir-map {"R" [0 1] "D" [1 0] "L" [0 -1] "U" [-1 0]})

(defn next-coords
  "Given a position triplet with in and out directions, the next direction
  and number of times to repeat the move return set set of new coordinates
  return a new set of coords with out direction from start filled in"
  [[start indir _] direction times]
  (loop [todo times
         res [[start indir direction]]]
    (if (> todo 0)
      (let [[latest indir _] (peek res)
            nxt-coord (mapv + latest (dir-map direction))]
        (recur (dec todo)
               (into (pop res)
                     [[latest indir direction] [nxt-coord direction nil]])))
      res)))

(comment
  (next-coords [[0 0] nil nil] "U" 4))
;; [[[0 0] nil "U"]
;;  [[-1 0] "U" "U"]
;;  [[-2 0] "U" "U"]
;;  [[-3 0] "U" "U"]
;;  [[-4 0] "U" nil]]

(defn add-to-border
  "Given a vector of coordinate triplets representing a partial border, 
  complete it with next, a pair of direction and times"
  [border next]
  (let [latest (peek border)
        [direction times _] next]
    (into (pop border) (next-coords latest direction times))))

(comment 
  (add-to-border [[[0 0] nil nil ] [[0 1] "D" "D"]] ["U" 3 "foo"]))

(defn get-edges
  "Get the min and max values for each dimension"
  [positions]
  (reduce (fn [m [x y]]
              (assoc m :min [(min (get (:min m) 0) x) (min (get (:min m) 1) y)]
                       :max [(max (get (:max m) 0) x) (max (get (:max m) 1) y)]))
          {:min [##Inf ##Inf] :max [##-Inf ##-Inf]}
          positions))

(comment
  (get-edges [[-4 1] [3 -1] [3 3] [1 9] [6 3]]))


(defn define-border
  "Given the input as a vector of triplets, create the (normalized) path as a map keyed by coord 
  with :in and :out directions from the instructions, includes the limits for retrieveing width and
  height"
  [input]
  (let [path (->> input
                  (reduce add-to-border [[[0 0] nil nil]]))
        start (path 0)
        {mins :min  maxs :max} (get-edges (map #(get % 0) path))
        norm-path (mapv (fn [[point i o]] (vector (mapv - point mins) i o)) path)]
        
    {:path (->> (assoc (pop norm-path) 0 [(start 0) ((peek norm-path) 1) (start 2)])
                (reduce #(assoc %1 (%2 0) (vector (%2 1) (%2 2))) {}))
     :limits (mapv  - maxs mins)}))


(comment
  (->> example-input parse-input define-border
       ((fn [m] (update-vals m #(take 3 %))))))
;; {:path ([[7 6] ["R" "D"]] [[7 1] ["U" "L"]] [[2 2] ["U" "L"]]),
;;  :limits (9 6)}
;; 

(defn intersect-path-right
  "Get the direction of the path that coord intersects horizontally to the right"
  [path-data coord]
  (let [width (get-in path-data [:limits 1])
        positions (:path path-data)]
    (loop [[r c :as current] coord]
      (if (> c width) nil
          (if-let [intersected (positions current)]
            intersected
            (recur [r (inc c)]))))))

(defn intersect-path-down
  "Get the directions of the path that coord intersects vertically down"
  [path-data coord]
  (let [height (get-in path-data [:limits 0])
        positions (:path path-data)]
    (loop [[r c :as current] coord]
      (if (> r height) nil
          (if-let [intersected (positions current)]
            intersected
            (recur [(inc r) c]))))))

(comment 
  (let [path (->> example-input parse-input define-border)]
    (map (partial intersect-path-down path) [[1 4] [4 1] [5 1]])))
    ;(get-in path [:path [5 4]])))

(defn area-path-intersections
  "Get all directions for intersections"
  [path-data]
  (let [[max-row max-col] (:limits path-data)
        path (:path path-data)
        intersections
        (for [r (range 0 (inc max-row))
              c (range 0 (inc max-col))]
              ;:when (not (path [r c]))]
          {[r c]
           (if (path [r c]) "border"
             {:right (intersect-path-right path-data [r c])
              :down  (intersect-path-down  path-data [r c])})})]
    (reduce conj intersections)))

(comment
  (->> example-input parse-input define-border
       area-path-intersections))

(defn outside-from-nils
  [int-dirs]
  (->> int-dirs
       (remove (fn [me] (or (every? #(not (nil? %)) (:in (val me)))
                            (every? #(not (nil? %)) (:out (val me))))))
       (map #(get % 1))))

(comment
  (->> example-input parse-input define-border
       area-path-intersections
       outside-from-nils))

(defn intersection-dirs
  [dirs]
  (->> dirs
       (map #(hash-map :right (hash-set ((:in %) 0) ((:out %) 0))
                       :down  (hash-set ((:in %) 1) ((:out %) 1))))
       (reduce (fn [L r] (merge-with into L r)))))

(comment
  (->> example-input parse-input define-border
       area-path-intersections
       outside-from-nils
       intersection-dirs))

(def outside-intersection-pairs
  {:right 
   {:clock #{["U" "U"]     ["U" "R"]  ["L" "U"]    nil}
    :counter #{["D" "D"] ["L" "D"] ["D" "R"] nil}}
   :down
   {:clock #{["R" "R"] ["R" "D"]  ["U" "R"]    nil}
    :counter #{["L" "L"] ["U" "R"] ["L" "D"] nil}}}) 

(defn clockwise?
  [intersections]
  (let [wallx (filter #(and
                         (map? (val %)) 
                         (or (nil? (:right (val %)))
                             (nil? (:down (val %)))))
                      intersections)]
    (cond (some (get-in outside-intersection-pairs [:right :clock])
                (into [] (map #(:right (val %)) wallx)))
          true
          (some (get-in outside-intersection-pairs [:down :clock])
                (into [] (map #(:down (val %)) wallx)))
          true
          :else false)))
   
  
(comment
  (->> example-input parse-input 
       ;(mapv #(assoc % 0 (case (% 0) "R" "L" "L" "R" (% 0))))
       define-border
       area-path-intersections))
       ;clockwise?))

(defn single-classify
  [clck v]
  (fn [v] (cond 
                 (= v "border") "border"
                 clck
                 (if (or ((get-in outside-intersection-pairs [:right :clock]) (:right v))
                         ((get-in outside-intersection-pairs [:down :clock])  (:down v)))
                     "OUTSIDE"
                     "INSIDE")
                 (not clck) 
                 (if (or ((get-in outside-intersection-pairs [:right :counter]) (:right v))
                         ((get-in outside-intersection-pairs [:down  :counter]) (:down v)))
                     "OUTSIDE"
                     "INSIDE")
                 :else "wtf")))

(defn classify 
  [path]
  (let [{border :path [H W] :limits } path
        intersections (area-path-intersections path)
        clck (clockwise? intersections)]
    (update-vals intersections
       (fn [v] (cond 
                (= v "border") "border"
                clck 
                (if (or ((get-in outside-intersection-pairs [:right :clock]) (:right v))
                        ((get-in outside-intersection-pairs [:down :clock])  (:down v))
                        (nil? (:right v)) (nil? (:down v))) 
                    "OUTSIDE"
                    "INSIDE")
                (not clck) 
                (if (or ((get-in outside-intersection-pairs [:right :counter]) (:right v))
                        ((get-in outside-intersection-pairs [:down  :counter]) (:down v))
                        (nil? (:right v)) (nil? (:down v))) 
                    "OUTSIDE"
                    "INSIDE")
                :else "wtf")))))
  

(comment 
  (def blocks (let [pth (->> example-input parse-input define-border)]
                (classify pth))))

; For the problem input it's returning +1 the actual result
(defn solution-1-wrong [s]
  (let [input (define-border (parse-input s))
        classified (classify input)]
    (count (remove #(= (val %) "OUTSIDE") classified))))
        
(comment
  (solution-1-wrong example-input) ;; 62
  (solution-1-wrong (slurp "resources/input/2023/input_18"))) ;; 47528 
;; real result is that -1 maybe a duplicate somewhere?

;; above-solution is wrong and a lot of code for nothing but we can print 
;; split the region and print the thing maybe even include the colors
(defn print-region [s]
  (let [pz-input s
        pz-data (parse-input pz-input)
        border (define-border pz-data)
        pth (:path border)
        intersections (area-path-intersections border)
        clck (clockwise? intersections)
        result (classify border)
        [H W] (:limits border)]
    (loop [c 0 r 0]
      (if (> c (inc W))
        (do (println "") (recur 0 (inc r)))
        (when (< r (inc H))
          (do
            (cond
              (= (result [r c]) "border") 
              (case (pth [r c]) 
                ["R" "R"] (print "-")
                ["L" "L"] (print "-")
                ["U" "U"] (print "|")
                ["D" "D"] (print "|")
                ["U" "R"] (print "/")
                ["D" "L"] (print "/")
                ["R" "D"] (print "\\")
                ["L" "U"] (print "\\")
                ["U" "L"] (print "\\")
                ["R" "U"] (print "/")
                ["D" "R"] (print "\\")
                ["L" "D"] (print "/"))
              (= (result [r c]) "INSIDE") (print "o")
              (= (result [r c]) "OUTSIDE") (print "."))
            (recur (inc c) r)))))))

;; simpler solution
;; TIL about shoelace formulae from:
;https://github.com/narimiran/AdventOfCode2023/blob/main/clojure/day18.clj
; damn so much easier
(defn shoelace-trapezoid
  "Computes area for a sequence of [direction, times]. Default assumes clockwise!"
  ([pts]
   (reduce (fn [acc [delta times]]
             (let [new (+ (:last acc) (* times (delta 1)))
                   narea (* times (delta 0) new)]
               {:area (+ (:area acc) narea (/ times 2)) :last new}))
           {:area 1 :last 0}
           pts))
  ([pts clockwise?] 
   (if clockwise? (shoelace-trapezoid pts)
       (reduce (fn [acc [delta times]]
                 (let [new (+ (:last acc) (* times (delta 0)))
                       narea (* times (delta 1) new)]
                   {:area (+ (:area acc) narea (/ times 2)) :last new}))
               {:area 1 :last 0}
               pts))))

(comment 
  (shoelace-trapezoid [[[1 0] 2] [[0 -1] 2] [[-1 0] 2] [[0 1] 2]]) 
  (shoelace-trapezoid [[[1 0] 2] [[0 1] 2] [[-1 0] 2] [[0 -1] 2]] false)) 

(defn solution-1 [s]
  (->> s
       parse-input 
       (mapv #(assoc % 0 (dir-map (% 0))))
       (#(shoelace-trapezoid % true))
       :area))

(comment 
  (solution-1 example-input)  ;; 62N
  (solution-1 (slurp "resources/input/2023/input_18"))) ;; 47527N

;; PART 2

(def int2dir {0 "R" 1 "D" 2 "L" 3 "U"})

(defn parse-line [v]
  (let [in v
        [_ _ h] in
        ch (s/replace h #"[()#]" "")
        [th ci] (split-at 5 ch)
        t (Long/parseLong (s/join th) 16) 
        c (int2dir (parse-long (apply str ci)))]
   [c t nil]))

(defn parse-input-2 [s]
  (->> s s/split-lines 
       (mapv #(s/split % #" "))
       (mapv parse-line))) 
   
(defn solution-2 [s]
  (->>
    (parse-input-2 s)
    (map #(assoc % 0 (dir-map (% 0))))
    shoelace-trapezoid
    :area)) 
;; {:area 52240187443190N, :last 0}


(comment 
  (solution-2 example-input) ;; 952408144115N
  (solution-2 (slurp "resources/input/2023/input_18"))) ;; 52240187443190N

