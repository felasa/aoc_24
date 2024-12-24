(ns day-23
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def example-input "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")

(defn parse-graph [edges]
  (let [reversed (map #(into [] (reverse %)) edges)
        directed-edge-list (into #{} (concat edges reversed))
        node-set (-> (group-by #(first %) directed-edge-list)
                     (update-vals 
                       (fn [edges] (into #{} (map (fn [edge] (edge 1)) edges)))))]
    {:node-set node-set :edge-set (set edges)})) ;directed-edge-list}))

(defn parse-input [s]
  (->> s string/split-lines
       (map #(string/split % #"-"))
       parse-graph))     

(comment 
  (parse-input example-input))

(defn find-thirds
  "Given pair of nodes, presumably an edge, find all the nodes in graph that 
  connect to both of them"
  [graph edge]
  (let [[base-node target-node] edge
        neighbors (get (:node-set graph) base-node)
        thirds (filter
                 (fn [node] (get (get (:node-set graph) node) target-node)) 
                 neighbors)]
    (into #{} thirds)))

(defn get-triplets 
  [graph edge]
  (let [thirds (find-thirds graph edge)
        base (set edge)]
    (map #(conj base %) thirds)))

(defn solution-1 [s]
  (let [graph (parse-input s)
        edges (:edge-set graph)
        triplets (distinct (mapcat (partial get-triplets graph) edges))]
    (count (filter (fn [triplet] (some (fn [node] (= (string/index-of node "t") 0)) triplet))
                   triplets))))

(comment 
  (solution-1 example-input) ;; 7
  (solution-1 (slurp "resources/input/input_23"))) ;; 1064
    
(defn s1 []
  (solution-1 (slurp "resources/input/input_23")))
  
;; PART 2

(defn find-extending
  "Given collection of nodes, presumably a complete graph, find nodes that create a n+1
  complete graph"
  [graph nodes]
  (let [neighbors (reduce set/intersection (map (fn [node] ((:node-set graph) node)) nodes))]
    neighbors)) 

(comment 
  (find-extending (parse-input example-input) ["ka" "co"]))

(defn extend-nodes
 "Return extended complete graph by 1 node if possible empty list if not"
 [graph nodes]
 (let [extra (find-extending graph nodes)]
   (map (partial conj nodes) extra)))

(comment 
  (extend-nodes (parse-input example-input) ["co" "de"]) ;; (["co" "de" "ka"] ["co" "de" "ta"])
  (extend-nodes (parse-input example-input) ["co" "de" "ta"]) ;; (["co" "de" "ta" "ka"])
  (extend-nodes (parse-input example-input) ["co" "de" "ta" "ka"])) ;; ()

(defn find-complete-subgraphs
  "Return the node-sets of all complete subgraphs of graphs"
  [graph]
  (loop [to-do (apply list (map set (:edge-set graph)))
         done #{}
         ret (set (map set (:edge-set graph)))]
    (if (empty? to-do) ret
      (let [[head & tail] to-do]
        (if (done head) 
          (recur tail done ret)
          (let [extended (extend-nodes graph head)]
            (if (empty? extended)
              (recur tail (conj done head) ret)
              (recur (apply (partial conj tail) extended)
                     (conj done head)
                     (apply (partial conj ret) extended)))))))))
          
(defn solution-2 [s]
  (->> s parse-input find-complete-subgraphs 
       (sort-by #(- (count %)))
       first
       sort
       (interpose ",")
       (apply str)))

(comment 
  (solution-2 example-input) ;; "co,de,ka,ta"
  (solution-2 (slurp "resources/input/input_23"))) ;; "aq,cc,ea,gc,jo,od,pa,rg,rv,ub,ul,vr,yy"

(defn s2 []
  (solution-2 (slurp "resources/input/input_23")))

