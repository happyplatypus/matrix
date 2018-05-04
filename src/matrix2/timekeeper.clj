(ns matrix.timekeeper (:require
  ;[matrix.tickers :as tickers]
                       [clojure.java.io :as io]
                       [clojure.string :as str]
                       [clj-http.client :as client]
                       [clj-time.format :as tf]
                       [clj-time.core :as tt]
                       [clojure.tools.nrepl.server :as serv]

                       [incanter.interpolation :refer :all]
                       [incanter [core :refer [$]
                                  :as incanter$]
                        [core :as incanter]
                        [stats :as stats]
                        [io :as io2]
                        [charts :as charts]
                        [datasets :as dataset]

                                        ;[interpolation :as interpolation]
]

                       [clojure.core.async
                        :as clojurecoreasync
                        :refer [>! <! >!! <!! go chan buffer close! thread
                                alts! alts!! timeout]]

                       [clojure.tools.cli :refer [cli]] [taoensso.timbre :as timbre]
                       [clojure.term.colors :refer :all])  (:use clojure.pprint)
    (:require [clojure.tools.cli :refer [parse-opts]])

    (:gen-class))

;(def time-now (zipmap (map keyword tickers/huge-tickers) (repeatedly (count tickers/huge-tickers) #(atom 0)         )))
(def global-time-now (atom 0.0))
