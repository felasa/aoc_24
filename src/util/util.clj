(ns util.util
  (:require [day-16 :refer [parse-input maze-graph neighbors-n-distance]]
            [clojure.math :as math]
            [clojure.data.priority-map :refer [priority-map]]))

(defn which
  "Returns the indixes where pred is true in coll"
  [pred coll]
  (keep-indexed (fn [idx item] (when (pred item) idx)) coll))

(defn gcd
 "greatest common divisor" 
  [a b]
  (loop [a (abs a) b (abs b)]
    (if (zero? b) a 
      (recur b (rem a b)))))

(defn lcm 
  "least common multiple"
  [a b]
  (if (or (zero? a) (zero? b)) 0
    (abs (* b (quot a (gcd a b)))))) 

(defn rel-rank 
  [coll n]
  (count (filter #(< % n) coll)))

;;TODO: Tie to the lowest
(defn rank
  "return the rank of elems in coll"
  [sq]
  (map (partial get (zipmap (sort sq) (range 1 (inc (count sq))))) sq))

(defn dist-l1
  "l1 (manhattan) distance"
  [x y]
  (reduce + (map (comp abs -) x y)))

(defn distance-ln 
  "L_{n} distance between x and y. L_{n} norm of x if y is not provided"
  ([n x] (distance-ln n x [0 0]))
  ([n x y]
   (let [inside-fn  (case n 1 identity ##Inf identity #(math/pow % n)) 
         reducing-fun (case n ##Inf max +)
         outside-fn (case n 1 identity ##Inf identity #(math/pow % (/ 1 n)))]
     (outside-fn (reduce reducing-fun (map (comp inside-fn abs -) x y))))))

(defn flat-to-coord
  "Translate flat coord to 2d coordinate [row col]"
  [width x]
  [(quot x width) (mod x width)])  

(comment 
  (map (partial flat-to-coord 3)
       (range 0 9))) ;; ([0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2])

(defn coord-to-flat
  "Translate 2d coord [row col] to flat position"
  [width coord]
  (+ (* (coord 0) width) (coord 1)))
  
(comment
  (map (partial coord-to-flat 3)
       (map (partial flat-to-coord 3) (range 0 9)))) ;; (0 1 2 3 4 5 6 7 8)

(defn reverse-graph
  "Reverses graph directions"
  [graph]
  (let [entries (for [[o nbs] graph [nb dist] nbs] [nb o dist])]
    (reduce (fn [m [nb o dist]] (assoc-in m [nb o] dist))
            {}
            entries)))
(defn queue
  "Queue constructor"
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([& args] (into (queue) args)))

(defn shortest-distances
  ([graph start] (shortest-distances graph start queue))
  ([graph start qtype] 
   (loop [q (if (= qtype priority-map) (qtype start 0) (qtype [start 0]))
          return {start 0}]
     (if (empty? q) return
       (let [[node weight] (peek q)
             neighbors (->> (update-vals (graph node) #(+ % weight))
                            (remove (fn [[nd w]] (> w (get return nd ##Inf)))))]
         (recur (into (pop q) neighbors) (into return neighbors)))))))

(defn shortest-distances-w-pred
  "shortest distances with predecessor"
  [graph start]
  (loop [queue (queue [start [nil 0]])
         visited {start [nil 0]}]
    (if (empty? queue) (update-vals visited #(apply hash-map %)) 
      (let [[current [prr dist]] (peek queue)
            neighbors-n-dist (map #(vector (% 0) [current (% 1)])
                                  (remove #(> (val %)
                                              (get-in visited [(key %) 1] ##Inf))
                                          (update-vals (graph current) #(+ dist %))))]
        (recur (into (pop queue) neighbors-n-dist) 
               (into visited neighbors-n-dist))))))

(defn shortest-distances-span 
  [sdwp]
  (reverse-graph (reverse sdwp)))

(comment 
  (let [G {:start {:a 1 :b 3} 
           :a {:b 1 :c 5 :d 1}
           :b {:c 1 :d 2}}]
    (shortest-distances G :start vector))
  (let [;s example-input
        s     (slurp "resources/input/input_16")
        data  (parse-input s)
        maze  (:maze data)
        start (:start data)
        end   (:end data)
        graph (maze-graph maze neighbors-n-distance)]
    (time (shortest-distances graph [[13 1] "E"] priority-map))
    (time (shortest-distances graph [[13 1] "E"] queue))
    (time (shortest-distances graph [[13 1] "E"] list))
    (time (shortest-distances graph [[13 1] "E"] vector))))

