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

(defn get-row [matrix row-num]
  (nth (seq matrix) row-num))

(defn make-prediction [Theta X]
  (cl/* X (cl/t Theta)))

(defn linear-regression [x Y a i]
  (let [m (first (cl/size Y))
        X (add-ones x)]
    (loop [Theta (cl/zeros 1 (second (cl/size X))) i i]
      (if (zero? i)
        Theta
        (let [ans (make-prediction Theta X)
              diffs (cl/- ans Y)
              dx (cl/* (cl/t diffs) X)
              adjust-x (cl/* dx (/ a m))]
          (recur (cl/- Theta adjust-x)
                   (dec i)))))))

(defn linear-regression' [x Y a max-iter]
  (let [m (first (cl/size Y))
        X (add-ones x)
        low-number 0.000001]
    (loop [Theta (cl/zeros 1 (second (cl/size X))) i 0 j-squared 0.0 last-error -1.0]
      (if (or #_(< (Math/abs (- j-squared last-error)) low-number)
           (>= i max-iter))
        {:theta Theta :mean-square-error j-squared :iterations i}
        (let [ans (make-prediction Theta X)
              diffs (cl/- ans Y)
              dx (cl/* (cl/t diffs) X)
              adjust-x (cl/* dx (/ a m))]
          (recur (cl/- Theta adjust-x)
                 (inc i)
                 (mse diffs)
                 j-squared))))))

(defn mean-normalization [inputs]
  (let [avg (/ (reduce + inputs) (count inputs))
        span (- (apply max inputs) (apply min inputs))
        normalize-fn #(/ (- % avg) span)]
    {:avg avg
     :span span
     :normalize normalize-fn
     :denormalize #(+ avg (* % span))
     :values (map normalize-fn inputs)}))

(defn denormalize [{:keys [avg span]} val]
  (+ avg (* val span)))

(defn denormalize-vec [{:keys [values] :as input}]
  (map (partial denormalize input) values))
