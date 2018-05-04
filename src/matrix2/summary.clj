(ns matrix.summary
  (:use [bigml.histogram.core])
  (:require (bigml.histogram.test [examples :as ex])))

(def hist (reduce insert! (create) ex/normal-data))

(def hist (reduce insert! (create) (identity 1)))

(def histo (create))
(insert! histo 1.0)

(insert! histo 2.0)

(insert! histo 3.0)

(percentiles hist 0.5 0.95 0.99)

;(ex/sum-density-chart histo)
