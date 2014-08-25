(ns goalcount.linear3
  (:require [goalcount.calcs :as c]
            [goalcount.energyfile :as e]))




(defn print-results [a scaled?]
  (let [X (if scaled? e/normalized-X e/X)
        results (c/linear-regression' X e/Y a 1000)]
    (println "scaled? " scaled?)
    (println "With learning rate:" a)
    (println "Error: " (:mean-square-error results))
    (println "iterations " (:iterations results))))


(print-results 0.0001 false)
(print-results 0.0001 true)

(print-results 0.00001 false)
(print-results 0.00001 true)

(print-results 0.000001 false)
(print-results 0.000001 true)

(print-results 0.0000001 false)
(print-results 0.0000001 true)

(print-results 0.00000001 false)
(print-results 0.00000001 true)

(print-results 0.000000001 false)
(print-results 0.000000001 true)
