(ns goalcount.core
  (:require [goalcount.statfile :refer [get-data get-values]]
            [incanter.charts :refer [scatter-plot add-lines]]
            [incanter.core :refer [view]]
            [incanter.stats :refer [linear-model]]
            [clojure.core.matrix :as m]
            [clatrix.core :as cl]))

(defn get-matrices [x-keys y-key]
  (let [data (get-data)
        X (get-values x-keys data)
        Y (get-values [y-key] data)]
    [X Y]))

(defn add-ones [x]
  (let [width (first (cl/size x))
        new-row (vec (repeat width 1))
        new-mat (cl/matrix new-row)]
    (cl/hstack new-mat x)))

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

(defn apply-hypothois [Theta X]
  (m/mmul X (m/transpose Theta)))

(defn get-square-error [guess real]
  (let [diffs (m/sub guess real)]
    (m/mul diffs diffs)))

(defn mse [diffs]
  (let [m (m/row-count diffs)]
    (-> (m/mul diffs diffs)
        (cl/sum)
        (/ (* 2 m)))))

(defn regress [x Y a i]
  (let [m (first (cl/size Y))
        X (add-ones x)]
    (loop [Theta  (cl/zeros 1 (second (cl/size X))) i i]
      (if (zero? i)
        Theta
        (let [ans (cl/* X (cl/t Theta))
              diffs (cl/- ans Y)
              dx (cl/* (cl/t diffs) X)
              adjust-x (cl/* dx (/ a m))
              ]
          (recur (cl/- Theta adjust-x)
                   (dec i)))))))

(def plot1 (scatter-plot (first goals) (second goals)))
#_(def ab (regress (first goals) (second goals) 0.0001 10000000))
(def plot2 (add-lines plot1 (first goals)
                      (:fitted (linear-model (second goals) (first goals)))))
#_(view plot2)
