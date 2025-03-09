(ns y2023.day-15
  (:require [clojure.string :as s]))

(def example-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn my-hash [s]
  (loop [cv 0
         remain (seq s)]         
    (if-let [cchar (first remain)]
      (recur (rem (* (+ cv (int cchar)) 17) 256)
             (rest remain))
      cv)))

(comment
  (my-hash "HASH"))  ;; 52

(defn solution-1 [s]
  (->> (-> s (s/replace "\n" "")
           (s/split #","))
       (map my-hash)
       (reduce +)))

(comment 
  (solution-1 example-input)  ;; 1320
  (solution-1 (slurp "resources/input/2023/input_15"))) ;; 517015
 
;; PART 2
;; this is the reducing function to apply the instructions
;; a priority map could do but ill keep track of insertion order and normalize later
(defn my-hashmap
  [L [idx r]]
  (let [[_ label op lens] (re-find #"(^\w+)([=-])(\d?)" r)
        box (my-hash label)]
    (cond  
      (= op "-") (assoc L box (dissoc (get L box {}) label))
      (= op "=") (if-let [og-ins (get-in L [box label :ins])]
                   (assoc-in L [box label] {:lens (parse-long lens) :ins og-ins})  
                   (assoc-in L [box label] {:lens (parse-long lens)
                                            :ins idx}))))) 

(defn rel-rank 
  "Relative rank of n within coll"
  [coll n]
  (count (filter #(< % n) coll)))

(defn normalize-insertion
  "adjust insertion order from 0 and no gaps"
  [box]
  (let [values (mapv #(:ins (val %)) box)]
    (update-vals box #(update-in % [:ins] (partial rel-rank values)))))

(defn power-lens 
  ;{k {:lens x :ins k},...}
  [lenses]
  (update-vals lenses #(* (inc (:ins %)) (:lens %))))

;; I'd like to make this cleaner
(defn solution-2
  [s]
  (let [s (s/replace s "\n" "")]
    (reduce + 
            (map
              #(* (inc (key %)) (val %)) 
              (update-vals ;;eww
                (update-vals 
                  (update-vals (reduce my-hashmap
                                      {}
                                      (map-indexed vector (s/split s #",")))
                             normalize-insertion)
                  power-lens)
                #(reduce + (vals %)))))))  

(comment 
  (solution-2 example-input) ;; 145
  (solution-2 (slurp "resources/input/2023/input_15"))) ;; 286104

