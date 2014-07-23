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

(->> (get-matrices [:for-h :for-a :against-h :against-a] :pts)
     reg-epl
     (print-results "forh-fora-againsth-againsta->pts"))

(->> (get-matrices [:for :against :played :gd :for-h :for-a] :pts)
     reg-epl
     (map *)
     (print-results "kitchen sink"))
