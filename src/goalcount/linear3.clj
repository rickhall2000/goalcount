(ns goalcount.linear3
  (:require [goalcount.calcs :as c]
            [goalcount.energyfile :as e]
            [clatrix.core :as cl]))

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
  (let [max-iter 10000
        results (if scaled?
                  (regress-with-scaling a max-iter)
                  (c/linear-regression' e/X e/Y a max-iter))]
    (println "--------------------")
    (println "With learning rate:" a)
    (println "scaled? " scaled?)
    (println "Error: " (:mean-square-error results))
    (println "iterations " (:iterations results))))

(defn get-err-for-alpha
  [scaled? alpha]
  (let [max-iter 1000]
    (if scaled?
      (:mean-square-error (regress-with-scaling alpha max-iter))
      (:mean-square-error (c/linear-regression' e/X e/Y alpha max-iter)))))

(def non-scaled-alphas
  [0.00000001
   0.00000002
   0.00000003
   0.00000004
   0.0000002
   0.0000004
   0.0000006
   0.0000008
   0.000001
   0.000002
   0.000003
   0.00000320
   0.00000335
   0.0000033771
   0.0000033772
   0.0000033773
   0.0000033774])

(def scaled-alphas
  [0.025
   0.05
   0.075
   0.10
   0.125
   0.15
   0.175
   0.2
   0.225
   0.25
   0.275
   1.75
   1.8
   1.9
   1.95
   1.998
   1.9985
   1.9999
   2.0])

(def non-scaled-errors
  (map (partial get-err-for-alpha false) non-scaled-alphas))

(def scaled-errors
  (map (partial get-err-for-alpha true) scaled-alphas))

(comment
  (print-results 0.025 true)
  (print-results 0.05 true)
  (print-results 0.075 true)
  (print-results 0.10 true)
  (print-results 0.125 true)
  (print-results 0.15 true)
  (print-results 0.175 true)
  (print-results 0.2 true)
  (print-results 0.225 true)
  (print-results 0.25 true)
  (print-results 0.275 true)
  (print-results 1.75 true)
  (print-results 1.8 true)
  (print-results 1.9 true)
  (print-results 1.95 true)
  (print-results 1.9998 true)
  (print-results 1.9999 true)
  (print-results 2.0 true)

  (print-results 0.00000001 false)
  (print-results 0.00000002 false)
  (print-results 0.00000003 false)
  (print-results 0.00000004 false)
  (print-results 0.0000002 false)
  (print-results 0.0000004 false)
  (print-results 0.0000006 false)
  (print-results 0.0000008 false)
  (print-results 0.000001 false)
  (print-results 0.000002 false)
  (print-results 0.000003 false)
  (print-results 0.00000320 false)
  (print-results 0.00000335 false)
  (print-results 0.0000033771 false)
  (print-results 0.0000033772 false)
  (print-results 0.0000033773 false)
  (print-results 0.0000033774 false)

  (def r (:stats e/normalized-X))
  (def x (first e/X))
  (map (fn [r x] ((:normalize r) x)) r x)
  )

;; Download the data and put into a Clojure vector
;; Make a clatrix matrix with the raw values of x
;; Make a function to normailze the different columns of x
;; apply the functions to the clojure vector to create a normalized X




;;;;;;;
(def scaled-model (regress-with-scaling 0.05 100000))
(def model (c/linear-regression' e/X e/Y 0.0000005 100000))

(def results
  (for [i (range (count e/Y))]
    [(cl/get (c/make-prediction (:theta model) (c/add-ones (c/get-row e/X i))) 0)
     (cl/get (c/make-prediction (:theta scaled-model) (c/add-ones (c/get-row (:X e/normalized-X) i))) 0)
     (nth e/Y i)]))

(defn square-error
  [x y]
  (let [diff (- y x)]
    (* diff diff)))

(defn avg-square-error
  [guesses actual]
  (let [errors (map square-error guesses actual)]
    (/ (reduce + errors) (count actual))))

(defn double-check [results]
  (let [guess1 (map #(nth % 0) results)
        guess2 (map #(nth % 1) results)
        real   (map #(nth % 2) results)]
    [(avg-square-error guess1 real) (avg-square-error guess2 real)]))
