(ns goalcount.core
  (:require [goalcount.statfile :refer [get-data get-values]]
            [goalcount.calcs :refer [regress]]
            [incanter.charts :refer [scatter-plot add-function]]
            [incanter.core :refer [view]]
            [incanter.stats :refer [linear-model]]
            [clatrix.core :as cl]))

(defn get-matrices [x-keys y-key]
  (let [data (get-data)
        X (get-values x-keys data)
        Y (get-values [y-key] data)]
    [X Y]))

(def goals (get-matrices [:for] :pts))
#_(def played (get-matrices [:played] :rank))
#_(def points (get-matrices [:pts] :rank))
#_(def wins (get-matrices [:win] :pts))

(defn plot-it [[X Y]]
  (view
   (scatter-plot X Y)))
(comment
  (plot-it goals)
  (plot-it played)
  (plot-it wins))

(def plot1 (scatter-plot (first goals) (second goals)))
(defn reg-epl [[X Y]]
  (regress X Y 0.0001 1000000))
(def goal-theta (reg-epl goals))
#_(def win-theta (reg-epl wins))
#_(def played-theta (reg-epl played))

(defn find-lm [[X Y]]
  (:coefs (linear-model Y X)))

(def goal-lm (find-lm goals))
#_(def win-lm (find-lm wins))
#_(def played-lm (find-lm played))

(defn plot-fn [x]
  (+ (* (second goal-theta) x) (first goal-theta)))

(def plot2 (add-function plot1 plot-fn 0 100))
#_(view plot2)
