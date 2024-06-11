(ns aoc-2 (:require [clojure.string :as str]))

(def initial-conditions {:red 12
                         :green 13
                         :blue 14})

; compara los valores de cada llave
;(def draw {:red 12 :green 1}) ;testcase
(defn draw-possible?
  "checks if [draw] is possible given (global) initial-conditions"
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
    #(assoc kv-map (keyword %2) (Integer/parseInt %1))
    entry))

; REFACTOR MAYBE?
(defn split-draws
  "Takes a sequence in de form of ['Game n' 'd1x1 red, d1x2 blue, d1x3 green'; ...]
  and creates a vector of (nested) maps representing the draws"
  [sq]
  (let [game (first sq)
        draws (last sq)]
   (conj
     (apply #(assoc {} (keyword %1) (Integer/parseInt %2)) (str/split game #" ")) 
     (assoc {} :draws (map (fn [vctr] (reduce add-to-draw {} (map #(str/split % #" ") vctr)))
                          (map #(str/split % #", ") ;list of vectors of color counts 
                                (str/split draws #"; "))))))) ; vector of draws 
  
(defn parse-input
  "read and parse input. output in the form:
   ({game: k, :draws ({:color n_1,..} {:color n_2,..} ...)}...)"
  [file]
  (map split-draws (map #(str/split % #": ")
                         (-> file 
                          slurp 
                          str/split-lines))))

(def data 
  (parse-input "input_2"))

(defn chk-all-draws [coll-of-draws] (every? true? (map draw-possible? coll-of-draws)))
(defn game-status [row] {:Game (:Game row) :valid (chk-all-draws (:draws row))})

(def solution 
  (reduce
    + 
    (map :Game (filter #(:valid %) (map game-status data)))))
(print solution)
(comment 
  (take 5 data))
