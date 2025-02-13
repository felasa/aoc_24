(ns day-19
  (:require [clojure.string :as s]))

(def example-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")

(defn parse-input [s]
  (let [sp (-> s s/split-lines)
        patterns (s/split (sp 0) #", ")
        designs (subvec sp 2)]
    {:patterns patterns :designs designs}))

(comment
   (parse-input example-input))

(defn sub-starting
  "return a collection of design with patterns removed from the start if applicable"
  [patterns design]
  (let [len (count design)
        subbed (map #(s/replace-first design % "") patterns)]
    (filter #(< (count %) len) subbed)))

;; To acount for orderings we check every tree of subsets until one is empty
;; For part 2 we could continue and count how many empties, but when to stop?
;; we stop when all are the empty string but its too expensive
;; this is slow and not used anymore
(defn design-possible? 
  "Returns if design is possible to form with patterns"
  [patterns design]
  (loop [to-do (list design)] 
    (if (not-every? #(not= % "") to-do) true
      (if (empty? to-do) false
        (recur (mapcat (partial sub-starting patterns) to-do))))))

;; Faster than the above 
(defn dp?
  [patterns design]
  ;(println {:patterns patterns :design design})
  (let [n-patterns (count patterns)
        patterns (vec patterns)]
    (loop [index 0
           s design
           used nil]
      ;(Thread/sleep 1000) (println {:i index, :s s, :u used})
      (if (= s "") true
        (if (>= index n-patterns)
          (if (empty? used) false
           (let [[last-used & tail] used]
             (recur (inc last-used) (str (patterns last-used) s) tail)))
          (let [pattern (patterns index)
                s' (s/replace-first s (re-pattern (str "^" pattern)) "")]
            (if (= s s') (recur (inc index) s used)
              (recur 0 s' (conj used index))))))))) 

; Makes solution bearable but probably the solution is not a very good one
; cant't even run one case without this
; dp is much faster but still too slow without trimming patterns
; ^not true anymore dp can run cases without trimming
(defn remove-redundant-patterns 
  "Removes from patterns all those that can be from other patterns"
  [patterns]
  (loop [n-pats 1
         patterns (sort-by count patterns)]
    (if (= n-pats (count patterns)) patterns
      (let [[pats des] (split-at n-pats patterns)]
        (recur (inc n-pats) 
               (concat pats (filter (complement (partial dp? pats)) des)))))))

(defn rrp 
  [patterns]
  (let [patterns (vec patterns)
        len (count patterns)]
    (loop [i 0
           return []]
      (if (>= i len) return
        (let [p (patterns i)]
          (if (dp? (into (subvec patterns 0 i) (subvec patterns (inc i))) p) 
            (recur (inc i) return)
            (recur (inc i) (conj return p))))))))

(comment 
  (let [;s example-input
        s (slurp "resources/input/input_19")
        data (parse-input s)
        patterns (:patterns data)]
    (time (println (remove-redundant-patterns patterns)))
    (time (println (rrp patterns)))))

; idea was to get multiplier on the redundants but we can get them on all the patterns
(defn keep-redundant-patterns 
  "Removes from patterns all those thaat can be from other patterns"
  [patterns]
  (let [minimal-patterns (into #{} (remove-redundant-patterns patterns))]
    (filter #(not (minimal-patterns %)) patterns)))    

(defn solution-1 
  [s]
  (let [;s example-input
        data (parse-input s)
        patterns (:patterns data)
        minimal-patterns (into [] (remove-redundant-patterns patterns))
        designs (:designs data)]
    (->> designs 
         (map (partial dp? minimal-patterns))
         (reduce (fn [l r] (if r (inc l) l)) 0)))) 
        

(comment 
  (solution-1 example-input) ;; 6
  (solution-1 (slurp "resources/input/input_19"))) ;; 317
;; PART 2
; copied solution
; didn't think about memoization
; things i tried:
; - remove redundant patterns and use only minimal ones. helped, but was still too slow.
; - assign multipliers to redundant patterns and use only those as base patterns
;   fails because a longer pattern can be broken down and form other valid patterns with 
;   adjacent characters
; - try to work out a dynamic programming style solution (ala coinchange problem).
;   got stack overflows
; - try to work out a formula for the problem broken down in substrings
;   may still be possible, couldnt figure it out.
; - The problem was identified correctly (too many patterns to try out) and a path to reduce
;   complexity;  some patterns are redundant and possible to be constructed out of the other, simpler ones. 
;   But this realization didnt bring forth the idea that once a redundant pattern is evaluated (and memoized)
;   the combinations of subpatterns that add to the same will have already been computed
; i.e. for patterns 'sbrgb' 's' 'brg' 'b' and design 'sbrgbxxxxxx' once one computes ways possible for 
; 'xxxxx' removing 'sbrgb' from the start you've already compueted for the combination of 's' 'brg' 'b'


; This could also be used to solve part1 and is faster
(def ways-possible
 (memoize 
   (fn np [patterns design]
     (if (empty? design) 1
       (->> design 
            ((fn [d] (filter #(s/starts-with? d %) patterns)))
            (transduce 
              (comp 
                (map #(subs design (count %)))
                (map #(ways-possible patterns %)))
              +))))))

(defn solution-2 [s] 
  (let [s (slurp "resources/input/input_19")
        data     (parse-input s)
        patterns (:patterns data)
        designs  (:designs data)]
    (->> designs
        (map (partial ways-possible patterns))
        (reduce +))))

(defn solution-1b [s]
  (let [
        data     (parse-input s)
        patterns (:patterns data)
        designs  (:designs data)]
    (count (remove zero? 
                  (map (partial ways-possible patterns)
                       designs))))) 
(comment 
  (solution-2 example-input) ;; 16
  (solution-2 (slurp "resources/input/input_19")) ;; 883443544805484
  (solution-1b (slurp "resources/input/input_19"))) ;; 317

(defn s2 [] 
  (solution-2 (slurp "resources/input/input_19")))

;; PREVIOUS (FAILED) ATTEMPTS FOR  PART 2
; can we make this function like dp? yes, see wdp
; dp breaks whenever design is formed, instead recur to start skipping an index?
(defn ways-design-possible 
  "Returns if design is possible to form with patterns"
  [patterns design]
  (loop [;iter 1
         to-do (list design)
         acc 0] 
    ;(Thread/sleep 1000) (flush) (prn to-do)
    ;(if (> iter 1) nil;
    (if (empty? to-do) acc
      (let [subbed (mapcat (partial sub-starting patterns) to-do)
            done-count (count (filter #(= "" %) subbed))]
         (recur subbed (+ acc done-count))))))

; faster but not enough to run on puzzle inputs
(defn wdp
  [patterns design]
  ;(println {:patterns patterns :design design})
  (let [n-patterns (count patterns)
        patterns (vec patterns)]
    (loop [index 0
           s design
           used nil
           acc 0]
      ;(Thread/sleep 1000) (println {:i index, :s s, :u used})
      (if (= s "") (recur (inc (first used))
                          (str (patterns (first used)) s)
                          (rest used)
                          (inc acc))
        (if (>= index n-patterns)
           (if (empty? used) acc
            (let [[last-used & tail] used]
              (recur (inc last-used) (str (patterns last-used) s) tail acc)))
           (let [pattern (patterns index)
                 s' (s/replace-first s (re-pattern (str "^" pattern)) "")]
             (if (= s s') (recur (inc index) s used acc)
               (recur 0 s' (conj used index) acc)))))))) 


;; with this we can reconstruct how the patterns are used for the design 
;; order of patterns matters we can try to check for the minimal or highly
;; redundant first
(defn dps
  "Return sequence of indexes of patterns used to form design"
  [patterns design]
  ;(println {:patterns patterns :design design})
  (let [n-patterns (count patterns)
        patterns (vec patterns)] 
    (loop [index 0
           s design
           used nil]
      ;(Thread/sleep 1000) (println {:i index, :s s, :u used})
      (if (= s "") used
        (if (>= index n-patterns)
          (if (empty? used) nil
           (let [[last-used & tail] used]
             (recur (inc last-used) (str (patterns last-used) s) tail)))
          (if (> (count (patterns index)) (count s)) (recur (inc index) s used)
            (let [pattern (patterns index)
                  s' (s/replace-first s (re-pattern (str "^" pattern)) "")]
              (if (= s s') (recur (inc index) s used)
                (recur 0 s' (conj used index)))))))))) 

(defn pattern-multipliers
  "Returns a map where they keys are the patterns and the value is the ways it can be 
   made of other patterns including itself"
  [patterns]
  (zipmap patterns 
          (map (partial wdp patterns)
               patterns)))

;; previous used ways-design-possible and input was the string input
;; todo: cleanup
(defn pattern-multipliers-2
  "Returns a map where they keys are the patterns and the value is the ways it can be 
   made of other patterns including itself"
  [patterns] 
  (zipmap patterns 
          (map (partial wdp patterns)
               patterns)))

; not good because a redundant pattern can be broken down in non disjoint sub patterns
; not really getting close
(defn solution-2fail 
 [s] 
 (let [;s (slurp "resources/input/input_19")
       data (parse-input s)
       patterns (:patterns data)
       designs (:designs data)
       minimal-patterns (vec (remove-redundant-patterns patterns))
       test-case (nth designs 2)
       min-construct (mapv minimal-patterns (reverse (dps minimal-patterns test-case)))]
    (->> designs
         (map (partial wdp patterns)))))

(defn wdp-from-minimal
  [patterns min-construct]
  ;(println min-construct)
  (let [min-construct (vec min-construct)
        len (count min-construct)]
    (if (<= len 3) (wdp patterns (apply str min-construct))
      (* (wdp-from-minimal patterns (subvec min-construct 0 (inc (quot len 2))))
         (wdp-from-minimal patterns (subvec min-construct (quot len 2)))))))                  

