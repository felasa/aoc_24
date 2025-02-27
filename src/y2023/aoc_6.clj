(ns y2023.aoc-6 
  (:require
    [clojure.string :as str]))

(def file "resources/input/2023/input_6")

(defn parse-input 
  [file]
  (->> 
    file 
    slurp
    str/split-lines
    (map #(rest (str/split % #" +")))
    (map #(map parse-long %))))

(defn distance
  [hold-time race-time]
  (* hold-time (- race-time (min hold-time race-time))))

(defn n-ways
  [race-time record]
  (let [options (range 1 race-time)]
    (count (filter #(> % record) (map #(distance % race-time) options)))))
 
(defn solution-1
  [file]
  (let [data (parse-input file)
        [times records] data]
    (->> 
      (map vector times records)
      (map #(apply n-ways %))
      (reduce *)))) 

(solution-1 file)
(defn parse-input-2 
  [file]
  (as-> 
    file x 
    (slurp x)
    (str/replace x #" +" "")
    (str/split-lines x)
    (map #(re-find #"\d+" %) x)
    (map parse-long x)))
      
;slow but w/e
(defn solution-2 
  [file]
  (apply n-ways (parse-input-2 file)))
 
(comment (solution-2 file))

