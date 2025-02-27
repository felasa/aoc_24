(ns day-25
  (:require [clojure.string :as string]))

(def example-input "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")

(defn lock-or-key 
  [lines]
  (let [top (first lines)
        bottom (last lines)]
    (if (and (every? #(= % \#) top)
             (every? #(= % \.) bottom))
      :lock
      :key)))

(defn heights
  [lock-pattern]
  (loop [todo (rest lock-pattern)
         ret (repeat (count (first lock-pattern)) 0)]
    (if (empty? todo) ret 
      (let [line (first todo)
            adding (map #(if (= % \#) 1 0) line)]
        (recur (rest todo) (map + ret adding))))))
        
(defn parse-input [s]
  (->> s string/split-lines 
       (partition-by #(= % ""))
       (filter #(> (count %) 1))
       (group-by lock-or-key)
       ((fn [m] (update-in m [:lock] (partial map heights))))
       ((fn [m] (update-in m [:key] (partial map (fn [key] (heights (reverse key)))))))))
       
(defn fit? 
  [heights1 heights2]
  (every? #(<= % 5) (map + heights1 heights2)))

(defn solution-1 
  [s]
  (let [data (parse-input s)
        locks (:lock data)
        keys (:key data)]
    (reduce + (for [lock locks
                    key keys
                    :let [fit (if (fit? lock key) 1 0)]]
                fit))))

(comment 
  (solution-1 example-input) ;; 3
  (solution-1 (slurp "resources/input/input_25"))) ;; 3021

