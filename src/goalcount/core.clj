(ns goalcount.core
  (:require [goalcount.statfile :refer [get-data get-values]]
            [incanter.charts :refer [scatter-plot add-lines]]
            [incanter.core :refer [view]]
            [clojure.core.matrix :as m]))

(defn get-matrices [x-keys y-key]
  (let [data (get-data)
        X (get-values x-keys data)
        Y (get-values [y-key] data)]
    [X Y]))

(def goals (get-matrices [:for] :pts))
(def played (get-matrices [:played] :rank))
(def points (get-matrices [:pts] :rank))
(def wins (get-matrices [:win] :pts))

(defn plot-it [[X Y]]
  (view
   (scatter-plot X Y)))
(comment
  (plot-it goals)
  (plot-it played)
  (plot-it points))



(defn trim [v]
  [(first v)
   (m/matrix :clatrix [(rest v)])])

(defn apply-hypothois [h X]
  (let [[theta0 theta-n] (trim h)
       hn (m/mmul X (m/transpose theta-n))]
    (m/add hn theta0)))

(defn get-square-error [guess real]
  (let [diffs (m/sub guess real)]
    (m/mul diffs diffs)))

(def h (m/matrix :clatrix [[1 2 3] ]))

(def x (m/matrix :clatrix [[1] [3] [5] [0]]))

(def y (m/matrix :clatrix [[3] [7] [11] [1]]))

(def ans (apply-hypothois h x))

(defn regress [X Y a n]
  (let [m (m/row-count Y)]
    (loop [Theta (vec (repeat (inc (m/column-count X)) 0)) n n]
      (if (zero? n)
        Theta
        (let [ans (apply-hypothois Theta X)
              diffs (m/sub ans Y)
              sum-dif (m/esum diffs)
              theta-0 (first Theta)
              theta-n (rest Theta)
              adjust-0 (* sum-dif (/ a m))
              dx (m/mmul (m/transpose diffs) X)
              adjust-x (m/mul dx (/ a m))
              ]
          (recur (reduce conj [(- theta-0 adjust-0)]
                         (m/to-vector (m/sub theta-n adjust-x)))
                   (dec n)))))))
