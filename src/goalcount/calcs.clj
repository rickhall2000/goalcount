(ns goalcount.calcs
  (:require [clatrix.core :as cl]))

(defn add-ones [x]
  (let [width (first (cl/size x))
        new-row (vec (repeat width 1))
        new-mat (cl/matrix new-row)]
    (cl/hstack new-mat x)))

(defn mse [diffs]
  (let [m (first (cl/size diffs))]
    (-> (cl/mult diffs diffs)
        (cl/sum)
        (/ (* 2 m)))))

(defn regress [x Y a i]
  (let [m (first (cl/size Y))
        X (add-ones x)]
    (loop [Theta (cl/zeros 1 (second (cl/size X))) i i]
      (if (zero? i)
        Theta
        (let [ans (cl/* X (cl/t Theta))
              diffs (cl/- ans Y)
              dx (cl/* (cl/t diffs) X)
              adjust-x (cl/* dx (/ a m))]
          (recur (cl/- Theta adjust-x)
                   (dec i)))))))
