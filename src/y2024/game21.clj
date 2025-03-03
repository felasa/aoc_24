(ns game21
  (:require [day-21 :as lib]
            [clojure.pprint :refer [pprint]])
  (:gen-class :main true))

(def init-state 
  {:numpad  {:robot-position \A :pressed "" :history "A"} 
   :dpad-1  {:robot-position \A :pressed "" :history "A"} 
   :dpad-2  {:robot-position \A :pressed "" :history "A"} 
   :dpad-3  {:pressed ""}})

(def numpad-graphic
  "+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+")

(def dpad-grpahic
  "    +---+---+
    | ^ | A |
+---+---+---+
| < | v | > |
+---+---+---+")

(defn update-dpad-state
  [state instruction]
  (let [position (:robot-position state)
        pressed (:pressed state)
        history (:history state)]
    (case instruction
      \A (assoc state :pressed (str pressed position) :history (str history \P))
      (if-let [newpos ((lib/dpad-travel-map position) instruction)]
        (assoc state :robot-position newpos :history (str history newpos))
        nil))))          
    
(defn update-numpad-state
  [state instruction]
  (let [position (:robot-position state)
        pressed (:pressed state)
        history (:history state)]
    (case instruction
      \A (assoc state :pressed (str pressed position) :history (str history \P))
      (if-let [newpos ((lib/numpad-travel-map position) instruction)]
        (assoc state :robot-position newpos :history (str history newpos))
        nil))))          
 
(defn move
  [state dir]
  (let [numpad (:numpad state)
        dpad-1 (:dpad-1 state) 
        dpad-2 (:dpad-2 state) 
        dpad-3 (:dpad-3 state) 
        dpad3-update (assoc dpad-3 :pressed (str (:pressed dpad-3) dir))
        dpad2-update (update-dpad-state dpad-2 dir)
        dpad1-update (if (= dir \A)
                       (update-dpad-state dpad-1 (:robot-position dpad-2))
                       (assoc dpad-1 :history (str (:history dpad-1) \.)))
        numpad-update (if (and (= dir \A) 
                               (= (dpad-2 :robot-position) \A))
                        (update-numpad-state numpad (:robot-position dpad-1))
                        (assoc numpad :history (str (:history numpad) \.)))]
      (if (and numpad-update dpad1-update dpad2-update) 
        (do (println (str "\033[31m" "Pressed " dir "\033[0m"))
            (assoc state :numpad numpad-update
                         :dpad-1 dpad1-update
                         :dpad-2 dpad2-update
                         :dpad-3 dpad3-update)) 
        (do (println "Invalid move. Try again") state))))

(defn main-loop 
  [state]
  (println (str "\033[4;31m" "STATUS:" "\033[0m"))
  (print "\033[33m") (println "#### NUMPAD #####") (pprint (state :numpad)) (println "\033[0m")
  (print "\033[34m") (println "#### DPAD 1 #####") (pprint (state :dpad-1)) (println "\033[0m")
  (print "\033[35m") (println "#### DPAD 2 #####") (pprint (state :dpad-2)) (println "\033[0m")
  (print "\033[36m") (println "#### DPAD 3 #####") (pprint (state :dpad-3)) (println "\033[0m")
  (print (str "\033[5;31m" "Enter direction: " "\033[0m"))
  (let [
        instruction (read-line)]
     (case instruction 
       "quit" (System/exit 0)
       "reset" (main-loop init-state)
       "setup" (main-loop (read-line))
       (do (flush) (main-loop (move state (first instruction)))))))

(defn -main [& args]
  (main-loop init-state))
