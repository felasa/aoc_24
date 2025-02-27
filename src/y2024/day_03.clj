(ns day-03)

(def example-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn solution-1
  [s]
  (->> s
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map #(* (parse-long (get % 1)) 
                (parse-long (get % 2))))
       (reduce +)))
            
(comment 
  (solution-1 example-input) ;; 161
  (solution-1 (slurp "resources/input/2024/input_03"))) ;; 183669043

(defn s1 []
  (println (solution-1 (slurp "resources/input/2024/input_03"))))

;; PART 2
(def example-input-2 
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64(mul(11,8)undo()?mul(8,5))))")

;; SHOULD BE A WAY USING map-reduce trickery
(defn solution-2 [s]
  (loop [rem (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)" s)
         ret 0
         do? true]
    (if (empty? rem) ret
      (let [current (first rem)
            next (rest rem)]
        (if do? 
          (if (get current 1)
            (recur next 
                   (+ ret 
                      (* (parse-long (get current 1)) 
                         (parse-long (get current 2))))
                   true)
            (if (= (get current 0) "don't()") 
              (recur next ret false)
              (recur next ret true)))
          (if (= (get current 0) "do()")
            (recur next ret true)
            (recur next ret false)))))))
                        
(defn s2 []
  (println (solution-2 (slurp "resources/input/2024/input_03")))) 

(comment 
  (solution-2 example-input-2) ;; 48
  (solution-2 (slurp "resources/input/2024/input_03"))) ;; 59097164
