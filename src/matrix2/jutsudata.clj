(ns matrix.jutsudata
  (:require [matrix.tickers :as tickers]))

;; jutsu needs a trace id map, to send updates to which trace?? Unf cant
;; do this by name of trace for now

(def trace-id-map (zipmap (map keyword (map first tickers/portfolio-name-tickers)) (repeatedly (count tickers/portfolio-name-tickers) #(atom {}))))

;(println trace-id-map)

;(swap! trace-id-map assoc-in [:AAPL] 1)
;(@trace-id-map :AAPL)
