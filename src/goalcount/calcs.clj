(ns goalcount.calcs
  (:require [clatrix.core :as cl]))

(defn add-ones "Add an X[0] column of all 1's to use with Theta[0]"
  [x]
  (let [width (first (cl/size x))
        new-row (vec (repeat width 1))
        new-mat (cl/matrix new-row)]
    (cl/hstack new-mat x)))

(defn mse [diffs]
  (let [m (first (cl/size diffs))]
    (-> (cl/mult diffs diffs)
        (cl/sum)
        (/ (* 2 m)))))

(defn linear-regression [x Y a i]
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

(defn linear-regression' [x Y a max-iter]
  (let [m (first (cl/size Y))
        X (add-ones x)
        low-number 0.01]
    (loop [Theta (cl/zeros 1 (second (cl/size X))) i 0 j-squared 0 last-error -1]
      (if (or (< (Math/abs (- j-squared last-error)) low-number)
           (> i max-iter))
        {:theta Theta :mean-square-error j-squared :iterations i}
        (let [ans (cl/* X (cl/t Theta))
              diffs (cl/- ans Y)
              dx (cl/* (cl/t diffs) X)
              adjust-x (cl/* dx (/ a m))]
          (recur (cl/- Theta adjust-x)
                 (inc i)
                 (mse diffs)
                 j-squared))))))

(defn mean-normalization [inputs]
  (let [avg (/ (reduce + inputs) (count inputs))
        span (- (apply max inputs) (apply min inputs))]
    {:avg avg
     :span span
     :values
     (map #(/ (- % avg) span) inputs)}))

(defn denormalize [{:keys [avg span values]}]
  (map #(+ avg (* % span)) values))
