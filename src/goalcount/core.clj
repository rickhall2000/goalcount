(ns goalcount.core
  (:require [goalcount.statfile :refer [get-data get-values]]
            [goalcount.calcs :refer [linear-regression]]
            [incanter.charts :refer [scatter-plot add-function]]
            [incanter.core :refer [view]]
            [incanter.stats :refer [linear-model]]
            [clatrix.core :as cl]))

;; This ns is just a scratch pad and will be removed at some point.
