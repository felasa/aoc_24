(ns y2023.day-13
  (:require [clojure.string :as s]))

(def example-input "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defn parse-input [s]
  (->> s
       s/split-lines
       (partition-by #{""})
       (remove #{[""]})
       (mapv vec)))

(def example-data (parse-input example-input))

(defn mirrored-at? 
  "checks if s is mirrored after index position"
  [s position]
  (let [cv (vec s)
        left  (subvec cv 0 position)
        right (subvec cv position)]
    (every? true? (map = (rseq left) right))))

(comment 
  (mirrored-at? "#.##..##." 4) ;; false
  (mirrored-at? "#.##..##." 5)) ;; true

(defn vmirror-position
  "return the column position where groun dis mirrored"
  [ground]
  (let [H (count ground)
        W (count (ground 0))]
    (->> (range 1 W)
         (map (fn [col] 
                (every? true? 
                        (map #(mirrored-at? (ground %) col)
                             (range 0 H)))))
         (map #(if (true? %2) %1 nil) (range 1 W))
         (some identity))))

(comment
  (vmirror-position (example-data 0)) ;; 5
  (vmirror-position (example-data 1))) ;; nil

(defn transpose 
  "Transppose 2d vector of vectors"
  [M]
  (let [H (count M)
        W (count (M 0))]
     (loop [c 0
            T (transient [])] 
       (if (< c W) 
         (recur (inc c) (assoc! T c (mapv #(get-in M [% c]) (range H))))
         (persistent! T)))))

(defn hmirror-position
  [ground]
  (vmirror-position (transpose ground)))

(comment
  (hmirror-position (example-data 1)) ;; 4
  (hmirror-position (example-data 0))) ;; nil

(defn solution-1 [s]
  (let [cases (parse-input s)
        columns (reduce + (remove nil? (map vmirror-position cases)))
        rows (reduce + (remove nil? (map hmirror-position cases)))]                
   (+ columns (* 100 rows)))) 

(comment
  (solution-1 example-input) ;; 405
  (solution-1 (slurp "resources/input/2023/input_13"))) ;; 29130

;; PART 2
(defn n-mirrored-at
  "Count how many rows are mirrored at col"
  [ground height col] 
  (->> (range height)
       (map #(mirrored-at? (ground %) col))
       (filter true?)
       (count)))

(defn n-mirrored
  "for every col, how many rows are mirrored at that pos"
  [ground]
  (let [H (count ground)
        W (count (ground 0))]
    (->> (range 1 W)
         (map (partial n-mirrored-at ground H)))))

(defn swap-symbol 
  "Changes # for . and viceversa at position"
  [cv position]
  (let [cv (vec cv)]
    (if (#{\.} (cv position))
      (assoc cv position \#)
      (assoc cv position \.))))

(comment 
  (swap-symbol "...###" 5)) ;; [\. \. \. \# \# \.]

(defn which
  "Returns indices where pred is true" 
  [pred coll]
  (loop [rem coll i 0 ret []]
    (if-let [current (first rem)]
      (if (pred current)
        (recur (rest rem) (inc i) (conj ret i)) 
        (recur (rest rem) (inc i) ret))
      ret)))

; dont like this but w/e
(defn vmirror-position-if-smudge
  [ground]
  (let [og-mirror-col (vmirror-position ground)
        H (count ground) W (count (ground 0))
        m-counts (n-mirrored ground)
        potential-mirror-cols (map inc (which #(= (dec H) %) m-counts))]
    (first
      (for [pmc potential-mirror-cols
            r-idx (range H)
            c-swp-idx (range W)
            :let [newground (assoc ground r-idx (swap-symbol (ground r-idx) c-swp-idx))]
            :when (and (every? true? (map #(mirrored-at? % pmc) newground))
                       (not= og-mirror-col pmc))]        
        pmc))))

(defn hmirror-position-if-smudge
  [ground]
  (vmirror-position-if-smudge (transpose ground)))

(defn solution-2 [s]
  (let [cases (parse-input s)
        columns (reduce + (remove nil? (map vmirror-position-if-smudge cases)))
        rows (reduce +(remove nil? (map hmirror-position-if-smudge cases)))]
    (+ columns (* 100 rows))))

(comment
  (solution-2 example-input) ;; 400
  (solution-2 (slurp "resources/input/2023/input_13"))) ;; 20368

