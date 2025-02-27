(ns day-09
  (:require [clojure.string :as s]))

(def example-input "2333133121414131402")

(defn parse-input 
  [s]
  (loop [i 0
         I 0
         file? true
         res []] 
    (if (>= i (count s)) res
      (let [len (Character/digit (get s i) 10)]  
        (if file? 
            (recur (inc i) (+ I len) false
                   (conj res [(quot i 2) len]))
                   ;(assoc-in res [:file] (conj (:file res) [(quot i 2)  len])))
            (recur (inc i) (+ I len) true
                   (conj res [nil len])))))))
                   ;(assoc-in res [:free] (conj (:free res) [nil  len]))))))))

(defn compact 
  [disk]
  (let [split (split-with #(get % 0) (if (get (last disk) 0) disk (drop-last disk)))]
    (if (empty? (get split 1)) nil
      (let [[_ free-size] (first (get split 1))
            [file file-size] (last (get split 1))
            L (vec (get split 0)) R (vec (drop-last (rest (get split 1))))
            delta (abs (- file-size free-size))]
        (cond 
          (< file-size free-size) (into (conj L [file file-size] [nil delta]) 
                                        R) 
          (= file-size free-size) (into (conj L [file file-size]) 
                                        R)
          (> file-size free-size) (into (conj L [file free-size]) 
                                        (conj R [file delta]))))))) 
;Stack overflow
(defn compact2
  [disk]
  (let [split (split-with #(get % 0) (if (get (last disk) 0) disk (drop-last disk)))]
    (if (empty? (get split 1)) disk
      (let [[_ free-size] (first (get split 1))
            [file file-size] (last (get split 1))
            L (vec (get split 0)) R (vec (drop-last (rest (get split 1))))
            delta (abs (- file-size free-size))
            compacted (cond 
                        (< file-size free-size) (into (conj L [file file-size] [nil delta]) 
                                                      R) 
                        (= file-size free-size) (into (conj L [file file-size]) 
                                                      R)
                        (> file-size free-size) (into (conj L [file free-size]) 
                                                      (conj R [file delta])))]
         (compact2 (if (get (last compacted) 0) compacted (drop-last compacted))))))) 
       
(defn fold
  [l r]
  [(+ (l 0) (* (r 0) (apply + (range (l 1) (+ (l 1) (r 1))))))
   (+ (l 1) (r 1))])

;SLOW!
(defn solution-1 
  [s]
  (get (->> s 
            parse-input
            ;compact2
            (iterate compact) 
            (take-while identity) 
            last
            (reduce fold)) 0))

(comment 
  (solution-1 example-input) ;; 1928
  (solution-1 (s/replace (slurp "resources/input/2024/input_09") "\n" ""))) ;; 6471961544878

(defn parse-input2 
  [s]
  (reduce into []
          (map-indexed (fn [idx c] (repeat (Character/digit c 10) 
                                           (if (even? idx) (quot idx 2) nil))) 
                       s)))

;tried this using transient ret, was only slightly faster
(defn fill-nills-with
  "Vec shoud not have trailing nils"
  [vec filler]
  (loop [fill-values filler
         idx 0
         ret vec]
    ;(println fill-values idx ret)
    (if (or (>= idx (count ret)) (empty? fill-values)) ret
      (if (peek ret)
        (if (nil? (ret idx)) 
          (recur (rest fill-values) (inc idx) 
                 (pop (assoc ret idx (first fill-values))))
          (recur fill-values (inc idx) ret))
       (recur fill-values idx (pop ret))))))

(defn tfill-nills-with
  "Vec shoud not have trailing nils"
  [vec filler]
  (loop [fill-values filler
         idx 0
         ret (transient vec)]
    ;(println fill-values idx ret)
    (if (or (>= idx (count ret)) (empty? fill-values)) (persistent! ret)
      (if (ret (dec (count ret)))
        (if (nil? (ret idx)) 
          (recur (rest fill-values) (inc idx) 
                 (pop! (assoc! ret idx (first fill-values))))
          (recur fill-values (inc idx) ret))
       (recur fill-values idx (pop! ret))))))

(defn solution-1a 
  [s]
  (let [disk (parse-input2 s)
        fill-vs (reverse (filter (complement nil?) disk))]
    ;(fill-nills-with disk fill-vs)
    (->> (fill-nills-with disk fill-vs)
         (map-indexed (fn [idx v] (* idx v)))
         (reduce +)))) 

(defn solution-1b 
  [s]
  (let [disk (parse-input2 s)
        fill-vs (reverse (filter (complement nil?) disk))]
    ;(fill-nills-with disk fill-vs)
    (->> (tfill-nills-with disk fill-vs)
         (map-indexed (fn [idx v] (* idx v)))
         (reduce +)))) 
(comment 
  (solution-1a example-input) ;; 1928
  (solution-1b example-input)) ;; 1928
  ; much faster peek and pop helped; cound try first method but assoc-ing into the parsed input

(defn s1 []
  (solution-1a (drop-last (slurp "resources/input/2024/input_09")))) ;; 6471961544878

;; PART 2

(defn parse-input3 
  [s]
  (loop [i 0
         I 0
         file? true
         res (hash-map)] 
    (if (>= i (count s)) res
      (let [len (Character/digit (get s i) 10)]  
        (if file? 
            (recur (inc i) (+ I len) false
                   (assoc res I [(quot i 2) len]))
                   ;(assoc-in res [:file] (conj (:file res) [(quot i 2)  len])))
            (recur (inc i) (+ I len) true
                   (assoc res I [nil len])))))))

;What is this monstruosity
(defn compact-blocks
  [blocks]
  (loop [ret (transient blocks)
         idx-seek 0
         keys-take (apply list (sort > (keys (filter #((val %) 0) blocks))))
         dont-fit #{}]
    ;(println idx-seek idx-take (get ret idx-take) (get (get ret idx-take) 0)) 
    (let [idx-take (peek keys-take)]
      (if (or (<= idx-take 0) (empty? keys-take)) 
         (persistent! ret) ;exit when reaching start
         (if (or (<= idx-take idx-seek) 
                 (contains? dont-fit (get (get ret idx-take) 1))) ;no room
           (recur ret 0 (pop keys-take) (conj dont-fit (get (get ret idx-take) 1)))
           (if (or (nil? (ret idx-seek))
                   ((complement nil?) ((ret idx-seek) 0)))
             (recur ret (inc idx-seek) keys-take dont-fit)
             (let [free-size (get (get ret idx-seek) 1)
                   file-size (get (get ret idx-take) 1)]
               (cond 
                 (< free-size file-size) (recur ret (inc idx-seek) keys-take dont-fit) 
                 (= free-size file-size) (recur (assoc! ret 
                                                        idx-seek (get ret idx-take) 
                                                        idx-take [nil file-size])
                                                0 (pop keys-take) dont-fit) 
                 (> free-size file-size) (recur (assoc! ret 
                                                        idx-seek (get ret idx-take)
                                                        (+ idx-seek file-size)
                                                        [nil (- free-size file-size)]
                                                        idx-take [nil file-size])
                                                0 (pop keys-take) dont-fit)))))))))

(defn solution-2 
  [s]
  (reduce + 
          (map (fn [[start [value len]]] (* value (reduce + (range start (+ start len)))))
               (filter #((val %) 0) (compact-blocks (parse-input3 s))))))

(comment 
  (solution-2 example-input) ;; 2858
  (solution-2 (s/replace (slurp "resources/input/2024/input_09") "\n" "")))  ;; 6511178035564
 
(defn s2 []
  (solution-2 (s/replace (slurp "resources/input/2024/input_09") "\n" "")))
