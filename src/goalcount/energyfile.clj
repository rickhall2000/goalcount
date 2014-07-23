(ns goalcount.energyfile
  (:require [clojure.string :as str]))

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
