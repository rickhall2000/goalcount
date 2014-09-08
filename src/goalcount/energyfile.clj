(ns goalcount.energyfile
  (:require [clojure.string :as str]
            [clatrix.core :as cl]
            [goalcount.calcs :as c]))

(def +energy-filename+ "ENB2012_data.csv")

(def read-file
  (->> (slurp +energy-filename+)
       (str/split-lines)
       (drop 1)
       (map #(str/split % #","))))

(defn make-record [line]
  (let [[rc sa wa ra ht ornt ga gad hl cl]
        (map read-string line)]
    {:relative-compactness rc
     :surface-area sa
     :wall-area wa
     :roof-area ra
     :overall-height ht
     :orientation ornt
     :glazing-area ga
     :glazing-area-distribution gad
     :heating-load hl
     :cooling-load cl}))

(def data
  (map make-record read-file))

(def X'
  (->> data
       (map (apply juxt [:overall-height :relative-compactness :orientation
                         :glazing-area-distribution :wall-area :glazing-area
                         :surface-area :roof-area]))
       vec))

(def X (cl/matrix X'))

(def normalized-X
  (let [rows (count X')
        cols (count (first X'))
        by-col (for [i (range cols)]
                 (map #(nth % i) X'))
        no-col (map c/mean-normalization by-col)
        vals (map :values no-col)
        step-1 (for [i (range rows)]
                 (map #(nth % i) vals))]
    {:X (cl/matrix step-1)
     :stats no-col}))

(def Y
  (->> data
       (map :heating-load)
       vec
       cl/matrix))
