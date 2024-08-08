(ns aoc-5 
  (:require
    [clojure.string :as str]))

(def file "example_5_1")

(defn split-blanks 
  [lines]
  (when-let [s (seq lines)]
    (let [[xs tys] (split-with (complement (partial = "")) s)
          ys (rest tys)]
      (if ys
        (cons xs (split-blanks ys))
        xs))))

(defn str-to-long 
  [str]
  (Long/parseLong str))

(defn parse-seeds 
  [string]
  (let [numbers (rest (str/split string #": +| +"))]
    (assoc {} :seeds (map str-to-long numbers))))

(defn create-map
  [code]
  (let [[dest orig len] code]
    (zipmap (range orig (+ orig len)) 
            (range dest (+ dest len)))))

(defn eval-map
  [m key]
  (get m key key))

(defn vec2lon 
  [vec]
  (map str-to-long vec))

(defn parse-map 
  [line]
  (let [map-id (first line)
        codes (rest line)]
    {(keyword (re-find #"[a-z\-]+" map-id))
     (->> 
        codes
        (map #(str/split % #" +"))
        (map #(map str-to-long %))
        (map create-map)
        (apply conj))}))

(defn parse-lines
  [lines]
  (let [h (first lines)
        tail (rest lines)]
    (conj  
      (parse-seeds (first h))
      (reduce merge (map parse-map tail)))))

(defn parse-input 
  [file]
  (-> file
      slurp
      str/split-lines
      split-blanks
      parse-lines))
      

(def data_l1 (parse-input file))
(keys data_l1)
;; (:seeds
;;  :seed-to-soil
;;  :soil-to-fertilizer
;;  :fertilizer-to-water
;;  :water-to-light
;;  :light-to-temperature
;;  :temperature-to-humidity
;;  :humidity-to-location)
(defn track-location 
  [data seed]
  (->> seed
      (eval-map (:seed-to-soil data))
      (eval-map (:soil-to-fertilizer data))
      (eval-map (:fertilizer-to-water data))
      (eval-map (:water-to-light data))
      (eval-map (:light-to-temperature data))
      (eval-map (:temperature-to-humidity data))
      (eval-map (:humidity-to-location data))))

(apply min (map (partial track-location data_l1) (:seeds data_l1))) ;; 35
;; out of memory when parsing data very innefficient solution
;(def puzzle-data (parse-input "input_5"))
;(apply min (map track-location (:seeds puzzle-data)))

;; retry using the straight minimal map codification and arithmetic
(defn parse-map2 
  [line]
  (let [map-id (first line)
        codes (rest line)]
    {(keyword (re-find #"[a-z\-]+" map-id))
     (->> 
        codes
        (map #(str/split % #" +"))
        (mapv #(mapv str-to-long %)))}))

(comment 
  (let [line (second (split-blanks (str/split-lines (slurp file))))]
    (parse-map2 line)))

(defn parse-lines2
  [lines]
  (let [h (first lines)
        tail (rest lines)]
    (conj  
      (parse-seeds (first h))
      (reduce merge (map parse-map2 tail)))))

(comment 
  (let [lines (split-blanks (str/split-lines (slurp file)))]
    (parse-lines2 lines)))

(defn between?
  "Return true if lb <= x <= ub"
  [x lb ub]
  (and (<= lb x) (>= ub x)))

(defn eval-map2
  [m v]
  (loop [remaining m]
    (if (empty?  remaining)
        v
        (let [[dest-start orig-start len] (first remaining)]
          (if (between? v orig-start (+ orig-start (dec len)))
            (+ dest-start (- v orig-start))
            (recur (rest remaining)))))))

(defn parse-input2 
  [file]
  (-> file
      slurp
      str/split-lines
      split-blanks
      parse-lines2))
      
(defn track-location2 
  [data seed]
  (->> seed
      (eval-map2 (:seed-to-soil data))
      (eval-map2 (:soil-to-fertilizer data))
      (eval-map2 (:fertilizer-to-water data))
      (eval-map2 (:water-to-light data))
      (eval-map2 (:light-to-temperature data))
      (eval-map2 (:temperature-to-humidity data))
      (eval-map2 (:humidity-to-location data))))

;; SOLUTION TO PART 1. LESSON LEARNED: DONT GET TOO FANCY CREATING MAPS, JUST APPLY THE RULE
;; (Maybe if they were lazy?)

(def example-data (parse-input2 "example_5_1"))
(def puzzle-data (parse-input2 "input_5"))
(apply min (map (partial track-location2 puzzle-data)  (:seeds puzzle-data)))

;;; PART 2
;; too slow for the input data again! iterating over waay too many (billions?) items in a sigle thread  
(defn min-seed-pair 
  [data pair]
  (let [[strt rng] pair]
    (loop [pos 0
           result nil]
      (if (= pos rng)
        result 
        (let [location (track-location2 data (+ strt pos))]
          (recur (inc pos)
                 (min location ((fnil identity location) result))))))))

(track-location2 example-data 82)
(min-seed-pair example-data [79 14]) ;; 46

(reduce min (map (partial min-seed-pair example-data)  
                 (partition 2 (:seeds example-data)))) ;; 46

(comment (reduce min (map (partial min-seed-pair puzzle-data)
                          (partition 2 (:seeds puzzle-data)))))
;;while i wait for the result.. how to intesect the intervals for a quicker map?
;every 
;; [seed-range ---- ]  --> [ soil-range ----] --> [fertilizer-range ----] etc...
;  [lb, ub] could be split if soil-map split
;; i.e.
;; [0,100] -> [1000 - 1100] but soil-map is [950 - 1050] [1051 - 1070]
;; can i map INTERVALS? instead of singletons?
;; while i mull it over and plug the laptop so it doesnt run out of battery maybe it'll 
;;  complete the earlier computation this year
;; ideas:
;;  compose define and compose the maps as functions, still bad because it would need too
;;    iterate over the seeds 
;;  instead map range or something? since they're mapped linearly they could be ordered by 
;;  their expected mapping [a,b] goes to [c,d] and [x y] goes to [w z] so if w < c [x y] 
;;    has to contain the minimum
;;  Maybe back-trace the piecewise ranges (inverse image of the different intervals)


;  d1                 (d1 + r1 -1)  
;   [--------------------]
;              [-----------------------------------------]
;              o2                                  (o2 + r2 -1)  
(defn intersect-maps  
  ;; MAL!! cuando la imagen de 1, no intersecta con el dominio no trivial de 2 
  ;; especificado debe usarse los mapeos de 2
  ;; para 2 funciona pero no sera valido con mas de 1
  ;; keeping the intersected bit only
  "composicion map1 -> map2 o map2(map1())"
  [map1 map2]
  (let [[d1 o1 r1] map1
        delta1 (- d1 o1)
        [d2 o2 r2] map2
        delta2 (- d2 o2)]
    (if (or (> d1 (+ o2 r2 -1)) (> o2 (+ d1 r1 -1))) ;CASE 1 no intersection of image and non-trivial domain
      ;[]
      ;[[d1 o1 r1]]
      [d2 o2 r2]
      (if (and (< d1 o2) 
               (<= o2 (- (+ d1 r2) 1)) 
               (<= (+ d1 r1) (+ o2 r2))) ; CASE 2: Intersection left side
        [
         [d1 o1 (- o2  d1)] ;left piece maps the same 
         [d2 (- o2 delta1) (- (+ d1 r1) o2)]] ;itersection maps as map2
         ;[(+ d2 delta2)  (+ d1 r1) (- (+ o2 r2) d1 r1)]] ;truncated version of map2
        (if (and (<= o2 d1) (<= (+ d1 r1) (+ o2 r2))) ;IMAGE proper subset of nontrivial domain 
          [[(+ d1 delta2) o1 r1]] ;intersection
           ;[d2 o2 (- d1 o2)]     ;left-side truncated map2 
           ;[(+ d1 r1 delta2) (+ d1 r1) (- (+ o2 r2) d1 r1)]] ;right side trunc. map2
          ;Int derecha
          [[(+ d1 delta2) o1 (- (+ o2 r2) d1)] ;parte que itersecta
           [ (+ o2 r2 delta2) (- (+ o2 r2) delta1) (- (+ d1 r1) o2 r2)]]))))) ;parte derecha
           ;[d2,(- o2 delta1), (- (+ d1 delta2) o2)]]))))) ;parte izquierda A 

(comment
  (intersect-maps '( 11 1 10) '( 100 13 24))
  (let [map1 [11 1 20] ;+10
        map2 [109 9 30]] ;+ 100
    (prn "Map 1: " map1)
    (prn "Map 2: " map2)
    (prn "Composed :" (intersect-maps map1 map2))))

(defn intersect-all 
  [ms m]
  (loop [orig m
         remaining ms
         result []]
    (let [intersection (intersect-maps m (first remaining))]
      (if (empty? remaining) result 
        (if (= 1 (count intersection))
          (if (identical? orig intersection)
            (recur orig (rest remaining) result)
            (into result intersection))
          (concat (recur (first intersection) (rest remaining) result) 
                  (recur (second intersection (rest remaining) result))))))))

(concat [[1 2]] [[1 3]])
(intersect-all 
  (:soil-to-fertilizer puzzle-data) 
  (first (:seed-to-soil puzzle-data)))


(intersect-maps (first (:seed-to-soil puzzle-data)) (second (:soil-to-fertilizer puzzle-data)))
(intersect-maps [50 98 2] [0 15 37])
;[98 99] ----> [50 51]
;           [15    51] ----> [0   36]
;[98 98] ------------------> [35 36]

(intersect-all 
  (:soil-to-fertilizer example-data) 
  (second (:seed-to-soil example-data)))
;[50   97] ----> [52       99]
;                [52 53]       ------> [37 38]
;[50 51] --> [37 38]
;[52 97] --> [54 99]
(intersect-maps [52 50 48] [37 52 2])
(+ 15 36)
(- 98 15)
(comment 
  (partition 3 (flatten (map (partial intersect-all (:soil-to-fertilizer example-data)) 
                          (:seed-to-soil example-data)))))
(partition 3 (flatten (map #(intersect-maps [11 1 10] %) [[100 13 24] [50 7 3]])))

(defn seed-to-location 
  [data]
  (loop [ks '(;:seed-to-soil
               :soil-to-fertilizer
              :fertilizer-to-water
              :water-to-light
              :light-to-temperature
              :temperature-to-humidity
              :humidity-to-location)
         result (:seed-to-soil data)]
    (if (empty? ks)
      result
      (recur (rest ks) 
             (distinct (partition
                            3 
                            (flatten 
                              (map (partial intersect-all result ) ((first ks) data))))))))) 
                                  
(take 5 (sort-by first (seed-to-location puzzle-data)))    
(defn track-location-test 
  [data seed]
  (->> seed
      (eval-map2 (:seed-to-soil data))
      (eval-map2 (:soil-to-fertilizer data))))
      ;(eval-map2 (:fertilizer-to-water data))
      ;(eval-map2 (:water-to-light data))
      ;(eval-map2 (:light-to-temperature data))
      ;(eval-map2 (:temperature-to-humidity data))
      ;(eval-map2 (:humidity-to-location data))))

(let [map1 (:seed-to-soil example-data)
      map2 (:soil-to-fertilizer example-data)]
  (prn "M1: " map1)
  (prn "M2: " map2)
  (prn "Comp: " (map (partial intersect-all map2) map1))
  (print "Comp first M1 and M2: " (intersect-all map2 (first map1))))

(def super-map (seed-to-location example-data)) ;; ((0 15 37) (37 52 2) (39 0 15) (39 0 11) (52 11 3))
(map (partial eval-map2 super-map) (:seeds example-data)) ;; (79 53 55 52)
(map (partial track-location-test example-data) (:seeds example-data)) ;; (81 53 57 52)

(:seeds example-data) ;; (79 14 55 13)
(:seed-to-soil example-data) ;; [[50 98 2] [52 50 48]]
(map (partial eval-map2 (:seed-to-soil example-data)) 
     (:seeds example-data))  ;; (81 14 57 13)
(:soil-to-fertilizer example-data) ;; [[0 15 37] [37 52 2] [39 0 15]]
(map (partial eval-map2 (:soil-to-fertilizer example-data)
              ()))

(comment 
  (let [data example-data
        seeds (:seeds example-data)
        seed-to-soil-map (:seed-to-soil example-data)
        soil-to-fertilizer-map (:soil-to-fertilizer example-data)]
    (do 
      (prn "Seeds: " seeds)
      (prn "Seed-Soil map: " seed-to-soil-map)
      (prn "Soils: " (map (partial eval-map2 seed-to-soil-map) seeds))
      (prn "Composed map" 
           (distinct 
             (partition
               3 
               (flatten 
                  (map 
                    (partial intersect-all soil-to-fertilizer-map) 
                    seed-to-soil-map))))))))
           
    

(map (partial intersect-all 
              (:seed-to-soil example-data)) 
     (:soil-to-fertilizer example-data))
;; (((0 15 37) (0 15 37))
;;  ((37 52 2) (37 52 2))
;;  ((39 0 15) (39 0 11) (52 11 4)))

(sort-by #(second %) (distinct (filter #(> (last %) 0) super-map)))
(sort-by #(first %) (:humidity-to-location puzzle-data))
;; mapear inversamente los destinos del ultimo mapa en orden?

;[d1 o1 r1] ^ [d2 s2 r2] 

;[o1, s1 + r1 - 1] ^ [d2, d2 + r2 - 1]

;o1 + r1 - 1 >= d2 y s1 <= d2 + r2 - 1 


(let [source [0 1459646077 120247692]
      targets (:temperature-to-humidity puzzle-data)])

(defn intrsct? 
  [m1 m2]
  (let [[d1 o1 r1] m1
        [d2 o2 r2] m2]
    (and (> (+ o1 r1) d1) (> (+ d2 r2) o1))))
    
(defn rev-intersect 
  [m-orig ms-dest]
  (sort-by first (filter #(intrsct? m-orig %) ms-dest)))

(let [m1 [0 1459646077 120247692]
      others (:light-to-temperature puzzle-data)]
  ;(sort-by first (filter #(intrsct? m1 %) others))
  (rev-intersect m1 others))

(defn reverse-traverse 
  [data]
  (loop [maps [:seed-to-soil            
               :soil-to-fertilizer
               :fertilizer-to-water
               :water-to-light
               :light-to-temperature
               :temperature-to-humidity]
               ;:humidity-to-location]
         origs (sort-by first (:humidity-to-location example-data))
         dests ((peek maps) data)
         path  [(first (sort-by first origs))]]
    (if (empty? maps)
      path
      (let [io (first origs)
            intersection (sort-by first (filter #(intrsct? io %) dests))]
        (if (empty? intersection)
          (recur maps (rest origs) dests ((second origs)))
          (recur (pop maps) intersection ((peek maps) data) (conj path (first intersection))))))))

(conj [1 2 3] 4)
(pop (conj [1 2 3] 4))

(seq (sort-by first (:humidity-to-location example-data)))            
(reverse-traverse example-data)    
(:humidity-to-location puzzle-data)
;[0 1459646077 120247692]

(def primeros-candidatos 
  (filter #(intrsct? (first (sort-by first (:humidity-to-location puzzle-data))) %) 
          (:temperature-to-humidity puzzle-data)))

(def segundos-candidatos (filter #(intrsct? (first (sort-by first primeros-candidatos)) %)
                                 (:light-to-temperature puzzle-data)))

(def terceros-candidatos (filter #(intrsct? (first (sort-by first segundos-candidatos)) %) ;; ()
                                 (:water-to-light puzzle-data)))

(def terceros-candidatos (filter #(intrsct? (second (sort-by first segundos-candidatos)) %)
                                 (:water-to-light puzzle-data)))

(def cuartos-candidatos (filter #(intrsct? (first (sort-by first terceros-candidatos)) %)
                                (:fertilizer-to-water puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (first (sort-by first cuartos-candidatos)) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (second (sort-by first cuartos-candidatos)) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (nth (sort-by first cuartos-candidatos) 2) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))

(def cuartos-candidatos (filter #(intrsct? (nth (sort-by first terceros-candidatos) 1) %) ;; ()
                                (:fertilizer-to-water puzzle-data)))

(def cuartos-candidatos (filter #(intrsct? (nth (sort-by first terceros-candidatos) 2) %) ;; ()
                                (:fertilizer-to-water puzzle-data)))

(def cuartos-candidatos (filter #(intrsct? (nth (sort-by first terceros-candidatos) 3) %)
                                (:fertilizer-to-water puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (first (sort-by first cuartos-candidatos)) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (nth (sort-by first cuartos-candidatos) 1) %)
                                (:soil-to-fertilizer puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (nth (sort-by first cuartos-candidatos) 2) %)
                                (:soil-to-fertilizer puzzle-data)))

(def cuartos-candidatos (filter #(intrsct? (nth (sort-by first terceros-candidatos) 4) %)
                                (:fertilizer-to-water puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (first (sort-by first cuartos-candidatos)) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (nth (sort-by first cuartos-candidatos) 1) %)
                                (:soil-to-fertilizer puzzle-data)))

(def cuartos-candidatos (filter #(intrsct? (nth (sort-by first terceros-candidatos) 5) %)
                                (:fertilizer-to-water puzzle-data)))

(def cuartos-candidatos (filter #(intrsct? (nth (sort-by first terceros-candidatos) 6) %)
                                (:fertilizer-to-water puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (first (sort-by first cuartos-candidatos)) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (nth (sort-by first cuartos-candidatos) 1) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))

(def quintos-candidatos (filter #(intrsct? (nth (sort-by first cuartos-candidatos) 2) %) ;; ()
                                (:soil-to-fertilizer puzzle-data)))
;(regreso un nivel...)
;etc...

(get (seq '(1 2)) 2)
(identical? (seq '(1 2)) '(1 2))
(type (seq '(1 2)))
(let [data puzzle-data
      mapas (apply vector (reverse (keys data)))]
  (loop [orig-idx 0 
         origenes (sort-by first ((get mapas orig-idx) puzzle-data))
         path (peek origenes)] 
      (let [
            destinos (sort-by first ((get mapas (inc orig-idx) puzzle-data)))
            intersection (filter #(intrsct? path %) destinos)]
        (if (= (inc orig-idx) 7) 
          path
          (if (empty? intersection)
            (recur orig-idx (rest origenes) (peek (rest origenes)))
            (recur (inc orig-idx))))))

 (defn rev-travel
   [data]
   (let [ks (into [] (keys data))]
     (loop [orig 7
            dest 6
            path (list)]
       (if (nil? ((get ks orig) data)) 
         path
         (if (nil? (filter #intrsct)) 
           (recur (rest orig) dest path)
           (recur (filter #(intrsct? path %) dest)))))))) 
                           
