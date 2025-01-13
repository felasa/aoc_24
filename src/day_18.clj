(ns day-18
  (:require [clojure.string :as s]))

(def example-input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")

(defn parse-input [s]
  (->> s s/split-lines
       (map #(s/split % #","))
       (mapv #(mapv parse-long %))))     

(comment 
  (parse-input example-input))

(defn graph 
  [data LEN n-bytes]
  (let [fallen (into #{} (subvec data 0 n-bytes))]
      (loop [col 0
             row 0
             graph (sorted-map)]
        (if (>= col LEN) graph
          (if (>= row LEN) (recur (inc col) 0 graph)
            (if (get fallen [col row]) (recur col (inc row) graph)
              (let [neighbors (transient {})]
                (doseq [dir [[-1 0] [1 0] [0 -1] [0 1]]
                        :let [n-row (+ row (dir 0)) n-col (+ col (dir 1))]
                        :when (and (<= 0 n-row (dec LEN)) (<= 0 n-col (dec LEN))
                                   (not (fallen [n-col n-row])))]
                   (assoc! neighbors [n-col n-row] 1))
                (recur col (inc row) (assoc graph [col row] (persistent! neighbors))))))))))

(defn draw-graph
  [dim graph]
  (doseq [
          row (range 0 dim)
          col (range 0 (inc dim))]
    (cond 
      (= col dim) (print "\n")
      (not (graph [col row])) (print "#")
      :else (print "."))))
    
    
(defn shortest-paths 
  [graph start]
  (loop [to-visit (list [start 0]) 
         return {}]
    ;(Thread/sleep 500) (println {:tv to-visit :v visited :r return})
    (if (empty? to-visit) return
      (let [current (first to-visit)
            [node distance] current
            neighbors (update-vals (graph node) #(+ distance %))]
          (if-let [pdist (return node)] 
            (if (< distance pdist) 
              (recur (into (rest to-visit) neighbors) 
                     (assoc return node distance))
              (recur (rest to-visit) 
                     return))      
            (recur (into (rest to-visit) neighbors) 
                   (assoc return node distance)))))))

(defn solution-1 [s]
  (let [data (parse-input s)
        graph (graph data 71 1024)]
   ((shortest-paths graph [0 0]) [70 70]))) 


(comment 
  (solution-1 (slurp "resources/input/input_18"))) ;; 294

;; PART 2

(defn sp-to-end 
  [data bytes]
  (let [g (graph data 71 bytes)]
    ((shortest-paths g [0 0]) [70 70])))

(defn solution-2 [s]
  (let [data (parse-input s)
        MAX_BYTES (+ -1 (count data))]
    (->> (range MAX_BYTES 1024 -1)
         (map-indexed (fn [idx v] [(- MAX_BYTES idx) (sp-to-end data v)]))
         (take-while #(nil? (% 1)))
         (last) (first) (+ -1)
         data
         (interpose ",")
         (apply str))))
                     
(comment 
  (solution-2 (slurp "resources/input/input_18"))) ;; "31,22"

