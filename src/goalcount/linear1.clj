(ns goalcount.linear1
  (:require [goalcount.statfile :refer [get-data get-values]]
            [goalcount.calcs :refer [linear-regression]]
            [incanter.charts :refer [scatter-plot add-function]]
            [incanter.core :refer [view]]
            [incanter.stats :refer [linear-model]]
            [clatrix.core :as cl]))

;; utility functions
(defn get-matrices [x-keys y-key]
  (let [data (get-data)
        X (get-values x-keys data)
        Y (get-values [y-key] data)]
    [X Y]))

(defn plot-it [[X Y]]
  (view
   (scatter-plot X Y)))

(defn reg-epl [[X Y]]
  (linear-regression X Y 0.0001 1000000))

(defn find-lm [[X Y]]
  (:coefs (linear-model Y X)))


;; Look at some data
(def wins (get-matrices [:win] :pts))
(plot-it wins)
(def win-theta (reg-epl wins))
(println "Wins-points: " win-theta)

(def played (get-matrices [:played] :rank))
(plot-it played)
(def played-theta (reg-epl played))
(println "played-rank: " played-theta)
(println "expected finish:" (+ (first played-theta)
                               (* 38 (second played-theta))))

(def goals (get-matrices [:for] :pts))
(plot-it goals)
(def goal-theta (reg-epl goals))
(def goal-lm (find-lm goals))
(println "goals-points: " goal-theta)
(println "goals-points (incanter): " goal-lm)

;; look at the data and regression together
(def goal-plot (scatter-plot (first goals) (second goals)))
(defn plot-fn [x]
  (+ (* (second goal-theta) x) (first goal-theta)))
(def plot-with-regression (add-function goal-plot plot-fn 0 100))

(view plot-with-regression)

(println "86 goals = " (+ (first goal-theta)
                          (* (second goal-theta) 86)))

(println "64 goals = " (+ (first goal-theta)
                          (* (second goal-theta) 64)))
