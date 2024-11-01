(ns aoc-8 
  (:require
    [clojure.string :as str]
    [criterium.core :as cr]))
  
(defn make-node
  "Make a node like {:node_0 {:left :node_l :right :node_r}} from input line"
  [line]
  (as-> 
    line x
    (str/replace x #"[^\w ]+" "")
    (str/split x #" +")
    {(keyword (get x 0)) {:left (keyword (get x 1)) :right (keyword (get x 2))}}))

(defn make-tree 
  "Given all the lines join into a single map"
  [desc-list]
  (reduce conj (map make-node desc-list)))

(defn transform-directions 
  "Transform the line of 'LR' directions into an array of :left :right keywords"
  [line]
  (as-> 
    line x 
    (str/split x #"")
    (map (fn [d] (if (= d "R") :right :left)) x)))

(defn parse-input
  "Parse the input as a map of directions and travel tree"
  [file]
  (as-> 
    file x
    (slurp x)
    (str/split-lines x)
    ;(take 20 x)
    (split-at 1 x)
    ;(get x 0)))
    {:directions (transform-directions (first (get x 0)))
     :tree (make-tree (rest (get x 1)))}))

(def puzzle-input (parse-input "resources/data/input_8"))
(def example-input (parse-input "resources/data/example_8_1"))
;; UNUSED, example of traversing the tree given directions
(defn travel 
  [directions tree start]
  (loop [remaining directions 
         current start]
    (if (empty? remaining)
      current 
      (let [direction (first remaining) 
            nxt (get (get tree current) direction)] 
          (recur (rest remaining) nxt)))))

(defn steps-until
  "Given directions start node, end node and a tree count the number of steps 
   until end node is reached, recycling directions if they end"
  [directions start end tree]
  (loop [remaining directions
         current start
         steps 0]
    (if (= current end)
      steps
      (if (empty? remaining)
        (recur directions current steps)
        (let [direction (first remaining)
              next-node (get (get tree current) direction)]
          (recur (rest remaining) next-node (inc steps)))))))
       
; SOLUTION TO PART 1
(steps-until (:directions puzzle-input) :AAA :ZZZ (:tree puzzle-input))

;;PART 2
;; so far tried constructing lazy seqs of both the paths for given starting
; points, indexing the number of steps and filtering by conditions 
; getting stack overflow errors unsure how to deal with it

;; match nodes ending wih A or Z
(comment (filter #(str/ends-with? (name (key %)) "A") (:tree puzzle-input))
         (filter #(str/ends-with? (name (key %)) "Z") (:tree puzzle-input)))

;; travel fun but lazy 
(defn lazy-travel 
  [directions start tree]
  (when directions 
    (let [head (get (get tree start) (first directions))] 
       (lazy-seq (cons head (lazy-travel (rest directions) head tree))))))

;;;; TRYING TO TRIM STACK ;;;;
(defn travel-s [tree from direction]
  (direction (from tree)))

(defn make-traveler [tree] 
  (partial travel-s tree))

(def puzzle-traveler (make-traveler (:tree puzzle-input)))
(def example-traveler (make-traveler (:tree example-input)))

(defn lzy-traveler
  "Return a fn that takes directions and starting point and returns seq of nodes"
  [tree]
  (fn rec-travel [directions start]
   (let [nxt ((first directions) (start tree))] 
    (lazy-seq (cons nxt
                    (rec-travel (rest directions) nxt))))))


(def pzle-traveler (lzy-traveler (:tree puzzle-input)))


(pzle-traveler [:left] :AAA)


(defn lzy-travel [start directions traveler]
  (when directions
    (let [nxt (traveler start (first directions))]
      (lazy-seq (cons nxt (lzy-travel nxt (rest directions) traveler))))))


(def test-1 (lzy-travel :AAA (cycle (:directions puzzle-input)) puzzle-traveler))
(def test-2 (lazy-travel (cycle (:directions puzzle-input)) :AAA (:tree puzzle-input)))
;(cr/bench (+ 1 1) :verbose)
;(cr/bench (apply vector (take 100000 test-1)))
;(cr/bench (apply vector (take 100000 test-2)))
(take 10 test-1)
(take 10 test-2)

(defn lazy-steps-until 
  [directions start condition tree]
  (map #(get % 0) (filter #(condition (get % 1)) 
                          (map-indexed (fn [idx itm] [idx itm]) 
                                       (lazy-travel directions start tree)))))

(defn lzy-steps-until 
  [directions condition start traveler]
  (map #(get % 0) (filter #(condition (get % 1)) 
                          (map-indexed (fn [idx itm] [idx itm]) 
                                       (lzy-travel start directions traveler)))))

(defn Z-node? [k] (str/ends-with? (name k) "Z"))

(let [start :AAA 
      directions (:directions puzzle-input)
      traveler puzzle-traveler
      condition Z-node?]
  (take 10 
        (filter #(Z-node? (% 1)) 
                (map-indexed (fn [idx itm] [(inc idx) itm]) 
                             (lzy-travel start directions traveler)))))

(def A-nodes (filter #(str/ends-with? (name %) "A") (keys (:tree puzzle-input))))

(defn lazy-travel-from [start] 
  (lazy-travel (cycle (:directions puzzle-input)) start (:tree puzzle-input)))

(defn steps-until-z [start] (lazy-steps-until (cycle (:directions puzzle-input))
                                              start 
                                              Z-node? 
                                              (:tree puzzle-input)))

(def foo (lzy-steps-until (:directions puzzle-input)
                          Z-node?
                          :AAA
                          puzzle-traveler))
                          
(first foo)
(def z-steps (map steps-until-z A-nodes))

(count (take 10000 (second z-steps)))

(first (drop-while (partial >= 78523) (second z-steps)))

(defn find-same 
  "to make sense sq1 and sq2 have to be monotonically increasing"
  [sq1 sq2]
  (let [bench (first sq1)
        possible (drop-while (partial > bench) sq2)]
    (if (= (first possible) bench)
      bench 
      (find-same (rest sq1) possible))))

(comment 
  (find-same (first z-steps) (first (rest z-steps)))
  (find-same (first z-steps) (first (rest (rest z-steps))))
  (find-same (first z-steps) (first (rest (rest (rest z-steps))))))

(defn find-common 
  [sq1 sq2 sq3]
  (let [bench (first sq1)
        ps2 (drop-while (partial > bench) sq2)
        ps3 (drop-while (partial > bench) sq3)]
    (if (= bench (first ps2) (first ps3))
      bench 
      (find-common (rest sq1) ps2 ps3))))

(defn find-common-2 
  [sq1 sq2 sq3]
  (let [bench (first sq1)
        ps2 (drop-while (partial > bench) sq2)
        ps3 (drop-while (partial > bench) sq3)]
    (if (= bench (first ps2))
      (if (= bench (first ps3))
        bench
        (find-common-2 (rest sq1) ps2 ps3))
      (find-common-2 (rest sq1) ps2 ps3))))

(find-common-2 
  (first z-steps)
  (second z-steps)
  (second (rest z-steps)))

(count (take 100 (first z-steps)))


(defn z-check [sq] (map Z-node? sq))

(def steps-till-z (fn [start] (lazy-steps-until (cycle (:directions puzzle-input)) 
                                                start 
                                                (fn [k] (str/ends-with? (name k) "Z"))
                                                (:tree puzzle-input))))
(def aaa-path (steps-till-z :AAA))
(def mja-path (steps-till-z :MJA)) 

                  
(def visited (lazy-travel  (cycle (:directions puzzle-input)) :AAA (:tree puzzle-input))) 
(last (take 19631 visited))
(take 1 (take-while #(= (last (name %)) \Z) visited))
(def con-N (filter #(= (last (name %)) \Z) visited))

