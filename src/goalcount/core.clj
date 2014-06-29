(ns goalcount.core
  (:require [goalcount.statfile :refer [get-data get-values]]
            [incanter.charts :refer [scatter-plot add-lines]]
            [incanter.core :refer [view]]))

(defn get-matrices [x-keys y-key]
  (let [data (get-data)
        X (get-values x-keys data)
        Y (get-values [y-key] data)]
    [X Y]))

(def goals (get-matrices [:for] :pts))
(def played (get-matrices [:played] :rank))
(def points (get-matrices [:pts] :rank))

(defn plot-it [[X Y]]
  (view
   (scatter-plot X Y)))

(plot-it goals)
(plot-it played)
(plot-it points)
