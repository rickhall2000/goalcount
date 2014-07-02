(ns goalcount.core
  (:require [goalcount.statfile :refer [get-data get-values]]
            [goalcount.calcs :refer [regress]]
            [incanter.charts :refer [scatter-plot add-function]]
            [incanter.core :refer [view]]
            [incanter.stats :refer [linear-model]]
            [clojure.core.matrix :as m]
            [clatrix.core :as cl]
            ))

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
  (plot-it points))


(def plot1 (scatter-plot (first goals) (second goals)))
(def ab (regress (first goals) (second goals) 0.0001 10000000))

(defn plot-fn [x]
  (+ (* (second ab) x) (first ab)))

(def plot2 (add-function plot1 plot-fn 0 100))
(view plot2)

(comment
  (doto (function-plot sin (- Math/PI) Math/PI)
    (add-function cos (- Math/PI) Math/PI)
    view))
