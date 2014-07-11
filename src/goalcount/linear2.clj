(ns goalcount.linear2
  (:require [goalcount.statfile :refer [get-data get-values]]
            [goalcount.calcs :refer [linear-regression]]))

(defn get-matrices [x-keys y-key]
  (let [data (get-data)
        X (get-values x-keys data)
        Y (get-values [y-key] data)]
    [X Y]))

(defn reg-epl [[X Y]]
  (linear-regression X Y 0.0001 1000000))

(defn print-results [label theta]
  (println (str "** " label " **"))
  (println theta))

(->> (get-matrices [:win] :pts)
    reg-epl
    (print-results "wins->points"))

(->> (get-matrices [:win :draw] :pts)
     reg-epl
     (print-results "wins+draws->points"))

(->> (get-matrices [:for] :pts)
     reg-epl
     (print-results "for->points"))

(->> (get-matrices [:for :against] :pts)
     reg-epl
     (print-results "for-against->pts"))


(defn fap-fn [{:keys [for against]}]
  (+ (* (second fap-theta) for)
     (* (last fap-theta) against)
     (first fap-theta)))

(def win-draw-loss-points (get-matrices [:win :draw :loss] :pts))
(def wdlp-theta (reg-epl win-draw-loss-points))
(println "** wdlp-theta **")
(println wdlp-theta)

(def for-against-gd-pts (get-matrices [:for :against :gd] :pts))
(def fagp-theta (reg-epl for-against-gd-pts))
(println "** fagp-theta **")
(println fagp-theta)
