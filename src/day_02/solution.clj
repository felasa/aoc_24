(ns day-02.solution
  (:require [clojure.string :as string])
  (:require [clojure.repl :as repl]))

(def input-path "resources/data/input_2")

(defn parse-draw
  "Turns a vector of separate cube counts in string form into a color->count map"
  [draw-vector]
  (as-> draw-vector x
       (map #(rseq (string/split % #" ")) x)
       (flatten x)
       (apply hash-map x)
       (update-vals x parse-long)
       (update-keys x keyword)))

(defn parse-game
  [game]
  (-> game
      (string/split #": ") ; Separate the game id
      (nth 1) ; Get the draw info
      (string/split #"; ") ; split the draws 
      (->>
       (mapv #(string/split % #", ")) ;split the cubes 
       (mapv parse-draw)))) ;make them maps

(defn read-input
  [path]
  (->> path
       slurp
       string/split-lines
       (map parse-game)))

(defn is-possible?
  "true if draw is possible given conditions, false if not"
  [draw conditions]
  (loop [to-check (keys conditions)]
    (if (empty? to-check)
      true
      (let [current (first to-check)]
        (if (every? #(>= (get conditions current 0) %)
                    (map #(get % current 0) draw))
          (recur (rest to-check))
          false)))))

(defn add-valid
  "Reducer to sum valid draws. x1 current tally x2 is [game-id valid-status]"
  [x1 x2]
  (if (x2 1)
    (+ x1 (x2 0))
    x1))

(defn solution-1 []
  (let [conditions {:red 12 :green 13 :blue 14}]
    (->> input-path
         read-input
         (map-indexed (fn [idx val] [(inc idx) (is-possible? val conditions)]))
         (reduce add-valid 0))))

(comment
  (solution-1)) ;; 2476
;;; PART 2 ;;;
(defn max-cubes 
  "Gets the maximum value for each color in a collection of draws"
  [draws]
  (loop [remaining draws
         result {:red 0 :green 0 :blue 0}]
    (if (empty? remaining)
      result
      (let [current (first remaining)]
        (recur (rest remaining) 
               (apply hash-map 
                 (flatten 
                   (map (fn [k] [k (max (get result k) (get current k 0))]) 
                        [:red :green :blue]))))))))
 ;             {:red (max (get result :red) 
 ;                        (get current :red 0)
 ;              :green (max (get result :green) 
 ;                          (get current :green 0))
 ;              :blue (max (get result :blue) 
 ;                         (get current :blue 0))))))))

(defn get-power 
  "Calculates the 'power' of a draw"
  [draw]
  (apply * (vals draw)))

(defn solution-2 []
  (->> input-path
       read-input
       (map max-cubes)
       (map get-power)
       (reduce +)))
(comment 
  (solution-2)) ;; 54911
