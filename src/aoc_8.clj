(ns aoc-8 
  (:require
    [clojure.string :as str]))
  
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

(def puzzle-input (parse-input "input_8"))

;; UNUSED, example of traverseing the tree give directions
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

