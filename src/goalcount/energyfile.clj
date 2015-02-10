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

(def X-seq
  (->> data
       (map (apply juxt [:overall-height :relative-compactness :orientation
                         :glazing-area-distribution :wall-area :glazing-area
                         :surface-area :roof-area]))
       vec))

(def X (cl/matrix X-seq))

(def normalized-X
  (let [rows (count X-seq)
        cols (count (first X-seq))
        by-columns (for [i (range cols)]
                 (map #(nth % i) X-seq))
        normalized-columns (map c/mean-normalization by-columns)
        vals (map :values normalized-columns)
        step-1 (for [i (range rows)]
                 (map #(nth % i) vals))]
    {:X (cl/matrix step-1)
     :stats (map #(dissoc % :values) normalized-columns)}))

(def Y
  (->> data
       (map :heating-load)
       vec
       cl/matrix))
