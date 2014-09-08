(ns goalcount.linear3
  (:require [goalcount.calcs :as c]
            [goalcount.energyfile :as e]))

(defn regress-with-scaling [a max-iter]
  (let [norm e/normalized-X
        X (:X norm)
        results (c/linear-regression' X e/Y a max-iter)
        theta' (:theta results)
        stats (map #(select-keys % [:avg :span])
                   (:stats norm))
        theta'' (reduce conj [(first theta')] (map c/denormalize stats (drop 1 theta')))]
    results))

(defn print-results [a scaled?]
  (let [max-iter 1000000
        results (if scaled?
                  (regress-with-scaling a max-iter)
                  (c/linear-regression' e/X e/Y a (* max-iter 10)))]
    (println "--------------------")
    (println "With learning rate:" a)
    (println "scaled? " scaled?)
    (println "Error: " (:mean-square-error results))
    (println "iterations " (:iterations results))))

(print-results 0.01 true)
(print-results 0.05 true)
(print-results 0.001 true)
(print-results 0.005 true)
(print-results 0.0005 true)

(print-results 0.000001 false)
(print-results 0.0000005 false)
(print-results 0.00000001 false)
(print-results 0.00000005 false)


;;;;;;;
(def scaled-model (regress-with-scaling 0.05 100000))
(def model (c/linear-regression' e/X e/Y 0.0000005 10000))

(println "*** unscaled predictions ***")
(println "row 1")
(println
 (c/make-prediction (:theta model) (c/add-ones (c/get-row e/X 1))))
(println "row 5")
(println
 (c/make-prediction (:theta model) (c/add-ones (c/get-row e/X 5))))
(println "row 10")
(println
 (c/make-prediction (:theta model) (c/add-ones (c/get-row e/X 10))))

(println "*** scaled predictions ***")
(println "row 1")
(println
 (c/make-prediction (:theta scaled-model)
                    (c/add-ones (c/get-row (:X e/normalized-X) 1))))
(println "row 5")
(println
 (c/make-prediction (:theta scaled-model)
                    (c/add-ones (c/get-row (:X e/normalized-X) 5))))
(println "row 10")
(println
 (c/make-prediction (:theta scaled-model)
                    (c/add-ones (c/get-row (:X e/normalized-X) 10))))
