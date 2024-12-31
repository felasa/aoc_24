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
;;  We count sides from the set of locations generated previously
;;  countinh the hirizontal ones first then the vertical ones
(defn count-hsides
  "Given a set of locations return the number of horizontal sides"
  [locations]
  (let [;locations (:locations (first (survey example-data)))
        min-row (reduce min (map #(% 0) locations)) 
        max-row (reduce max (map #(% 0) locations)) 
        min-col (reduce min (map #(% 1) locations))
        max-col (reduce max (map #(% 1) locations))]
    (loop [row min-row
           col min-col
           sides 0
           counting-top? false
           counting-bottom? false]
      ;(println {:coord [row col] :sides sides :ct counting-top? :cb counting-bottom?})
      (if (> row max-row) sides
        (if (> col max-col)
          (recur (inc row) min-col sides false false)
          (if (locations [row col])
            (let [neighbor-top (locations [(dec row) col])
                  neighbor-bottom (locations [(inc row) col])
                  add-top?    (not (or neighbor-top counting-top?))
                  add-bottom? (not (or neighbor-bottom counting-bottom?))]
               (recur row (inc col) 
                      (+ sides (if add-top? 1 0)
                               (if add-bottom? 1 0))
                      (if neighbor-top false true)
                      (if neighbor-bottom false true)))
            (recur row (inc col) sides false false)))))))

;Just reverse the coords
(defn count-vsides 
  "Given a set of locations count the vertical sides"
  [locations]
  (count-hsides (into #{} (map #(into [] (reverse %)) locations))))

(defn count-sides 
  "Returns the total count of sides (+ horizontal vertical)"
  [locations]
  (+ (count-hsides locations) (count-vsides locations)))

(defn add-sides
  "Adds a key of sides to a map containing the locations"
  [surveyed]
  (assoc surveyed :sides (count-sides (:locations surveyed))))

(defn bulk-cost 
  "Computs fencing bulk cost"
  [surveyed]
  (* (:area surveyed) (:sides surveyed)))

(defn solution-2
  [s]
  (->> s parse-input survey 
       (map add-sides)
       (map bulk-cost)
       (reduce +)))

(comment 
  (solution-2 example-input) ;; 1206
  (solution-2 (slurp "resources/input/input_12"))) ;; 784982

(defn s2 []
  (solution-2 (slurp "resources/input/input_12")))

