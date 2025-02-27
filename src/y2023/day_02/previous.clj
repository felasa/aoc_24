(ns y2023.day-02.previous 
  (:require [clojure.string :as string]))

(def initial-conditions {:red 12
                         :green 13
                         :blue 14})

; compara los valores de cada llave
;(def draw {:red 12 :green 1}) ;testcase
;A draw is a color->number map
(defn draw-possible?
  "checks if draw is possible given (global) initial-conditions"
  [draw]
  (every? true?
          (map #(apply (fnil <= 0) %) ; when none is drawn default to zero
               (map vector
                    (map draw [:red :green :blue])
                    (map initial-conditions [:red :green :blue])))))

(defn add-to-draw
  "Creates a map for the draw. 
  Draw is a seq of [n color] pairs"
  [kv-map entry]
  (apply
    #(assoc kv-map (keyword %2) (parse-long %1))
    entry))

; REFACTOR MAYBE?
(defn split-draws
  "Takes a sequence in the form of ['Game n' 'd1x1 red, d1x2 blue, d1x3 green'; ...]
  and creates a vector of (nested) maps representing the draws"
  [sq]
  (let [[game draws] sq
        [gkey id] (string/split game #" ")]
   (conj
     (array-map :Game (parse-long id))
     (assoc {} :draws (map (fn [vctr] (reduce add-to-draw {} (map #(string/split % #" ") vctr)))
                          (map #(string/split % #", ") ;list of vectors of color counts 
                                (string/split draws #"; "))))))) ; vector of draws 

(comment 
  (type (split-draws ["Game 1" "10 blue, 9 red, 2 green; 1 red"]))
  (->> "resources/data/input_2"
       slurp
       string/split-lines
       (map #(string/split % #": "))))

(defn parse-input
  "read and parse input. output in the form:
   ({game: k, :draws ({:color n_1,..} {:color n_2,..} ...)}...)"
  [file]
  (->> file 
       slurp 
       string/split-lines
       (map #(string/split % #": "))
       (map split-draws))) 

(def data 
  (parse-input "resources/input/2023/input_2"))

(defn chk-all-draws [coll-of-draws] (every? true? (map draw-possible? coll-of-draws)))
(defn game-status [row] {:Game (:Game row) :valid (chk-all-draws (:draws row))})
;;PART 1 SOLUTION
(def solution 
  (reduce
    + 
    (map :Game (filter #(:valid %) (map game-status data)))))
(comment
  solution print) ;; 2476
;; -- END PART 1 --

;for testing
(def example-data (parse-input "resources/input/2023/example_2_2")) 
(def test-case (:draws (first data)))
;;awful hack to deal with nils
(defn add-key-if-none 
  [kv k]
  (if (nil? (k kv))
    (assoc kv k 0)
    kv))

;; out of steam, there has to be a more concise way, not that I like this hack
;; update: structs?
(defn complete-draw
  [draw]
  (-> draw 
      (add-key-if-none :green)
      (add-key-if-none :blue)
      (add-key-if-none :red)))
  
(defn min-cubes 
  [draws]
  (zipmap [:red :blue :green]
          (map #(get (apply max-key % (map complete-draw draws)) %) 
               [:red :blue :green])))

(defn game-fewest-cubes 
  [game]
  {:Game (:Game game) :fewest (min-cubes (:draws game))})

(defn get-power
  [draw]
  (reduce * (vals draw)))

(def solution_2
  (reduce
    + 
    (map get-power 
         (map :fewest 
              (map game-fewest-cubes 
                   data)))))

(comment 
  solution_2) 
;; 54911
;; 54911
