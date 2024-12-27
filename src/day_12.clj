(ns day-12
  (:require [clojure.string :as string]))

(def example-input "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

(defn parse-input [s]  
  (->> s string/split-lines
       (mapv #(mapv identity %))))

(def example-data (parse-input example-input))

(defn get-char-at-pos
  [data row col]
  (if-let [row (get data row)]
    (get row col)
    nil))

(defn neighbors 
  [row col]
  [[(dec row) col] [row (inc col)] [(inc row) col] [row (dec col)]])

;; eww
(defn survey
  [data]
  (loop [row 0 col 0
         visited #{}
         current-plot nil
         current-to-do #{}
         result []]
    (if (>= row (count data)) result
      (if (>= col (count (first data)))
        (recur (inc row) 0 visited current-plot current-to-do result)
        (if (nil? current-plot)
          (if (contains? visited [row col]) 
            (recur row (inc col) visited current-plot current-to-do result) 
            (let [type (get-char-at-pos data row col)
                  neighbors (neighbors row col)
                  same-type (filter #(= type (apply (partial get-char-at-pos data) %))
                                    neighbors)
                  not-same (filter #(not= type (apply (partial get-char-at-pos data)
                                                      %))
                                   neighbors)
                  perimeter (count not-same)
                  new (filter (complement #(contains? visited %)) same-type)]
              (recur row col 
                     (conj visited [row col])
                     {:type type :locations #{[row col]}
                      :area 1 :perimeter perimeter}
                     (into current-to-do new)
                     result)))
          (if (empty? current-to-do)
            (recur row col visited nil current-to-do (conj result current-plot))
            (let [type (:type current-plot)
                  current (first current-to-do)
                  neighbors (apply neighbors current)
                  same-type (filter #(= type (apply (partial get-char-at-pos data) %))
                                    neighbors)
                  not-same (filter #(not= type (apply (partial get-char-at-pos data)
                                                      %))
                                   neighbors)
                  perimeter (+ (get current-plot :perimeter ) (count not-same))
                  area (+ (get current-plot :area ) 1)
                  new (filter (complement #(contains? visited %)) same-type)
                  locations (conj (:locations current-plot) current)]
              (recur row col (conj visited current)
                     {:type type :locations locations :area area :perimeter perimeter}
                     (into (disj current-to-do current) new)
                     result))))))))

(defn solution-1 
  [s]
  (->> s parse-input 
       survey
       (map (fn [plot] (* (:area plot) (:perimeter plot))))
       (reduce +)))

(comment 
  (solution-1 example-input) ;; 1930
  (solution-1 (slurp "resources/input/input_12"))) ;; 1319878

(defn s1 []
  (solution-1 (slurp "resources/input/input_12")))

;; PART 2
;;  should be able to count sides from the locations generated prev
(sort (update-vals (group-by #(% 0) (:locations (first (survey example-data)))) sort))


