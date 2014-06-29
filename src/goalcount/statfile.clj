(ns goalcount.statfile
  (:require [net.cgrand.enlive-html :as html]
            [clojure.core.matrix :as m]))

(def url-base "http://www.statto.com/football/stats/england/premier-league/")
(def url-end "/table")

(def start 1995)
(def finish 2014)

(def seasons
  (for [i (range start finish)]
    (str i "-" (inc i))))

(def urls
  (map (fn [yr] (str url-base yr url-end)) seasons))

(def line-fields
  [:rank :team :played :space
   :win :draw :loss :for :against :space
   :win-h :draw-h :loss-h :for-h :against-h :space
   :win-a :draw-a :loss-a :for-a :against-a :space
   :gd :pts :space :space])

(defn read-page [url]
  (let [dom (html/html-resource (java.net.URL. url))
        main (html/select dom [:div#content])
        rows (html/select main [:tr])]
    (drop 2 rows)))

(defn clean-value [raw]
  (let [unmapped (if (map? raw) (:content raw) raw)
        unlisted (if (seq? unmapped) (first unmapped) unmapped)
        unstrung (if (string? unlisted) (read-string unlisted) unlisted)]
    (if (number? unstrung) unstrung 0)))

(defn parse-line [raw-line]
  (let [line (html/select raw-line [:td])
        values-raw (map (comp first :content) line)
        values (map clean-value values-raw)]
    (dissoc
     (zipmap line-fields values)
     :space)))

(defn get-data []
  (mapcat read-page urls))

(defn get-values [keys data]
  (->> data
       (map parse-line)
       (map (apply juxt keys))
       vec
       (m/matrix :clatrix)))
