(ns matrix.processdata
  (:require
   [matrix.utils :as utils]

   [net.cgrand.enlive-html :as enlive]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [incanter.zoo :as zoo]
   [clj-time.format :as tf]
   [clj-time.core :as tt]
   [clj-time.predicates :as pr]
   [clj-time.local :as l]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clj-http.client :as client]
                                        ;  [structure.ring-buffer :as rb]
                                        ;  [postal.core :as postal]

   [clojure.core.async
    :as a
    :refer [>! <! >!! <!! go chan buffer close! thread
            alts! alts!! timeout]]
   [incanter [core :refer [$]
              :as incanter$]
    [core :as incanter]
    [stats :as stats]
    [io :as io2]
    [charts :as charts]
    [datasets :as dataset]]

   [me.raynes.conch :refer [programs with-programs let-programs] :as sh] [matrix.utils :as utils]) (:use clojure.pprint)
  ;(:use [clj-highcharts.core])

  (:gen-class))

;; initialize nasdaq and nyse ticker, our trading universe
(do (def HOME utils/HOME)
    (def mc-cutoff 800E6)
    (def price-cutoff 5)
    (def adv-cutoff-millions 10)
    (def urls ["http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download" "http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download"])
    (defn retrieve-symbols
      " use the above urls to retrieve ticker list for nyse and nasdaq, cutoff at certain points"
      [mc-cutoff price-cutoff adv-cutoff-millions url] (let [data (io2/read-dataset url :header true)
                                                             nasdaq (incanter/to-dataset data)
                                                             symbols (incanter$/$ :Symbol nasdaq)
                                                             name_ (incanter$/$ :Name nasdaq)
                                                             lastsale (incanter$/$ :LastSale nasdaq)
                                                             market-cap (incanter$/$ :MarketCap nasdaq)
                                                             sector (incanter$/$ :Sector nasdaq)
                                                             industry (incanter$/$ :Industry nasdaq)
                                                             filter-data0 (map vector symbols name_ lastsale market-cap sector industry)
                                                             filter-data1 (filter #(number? (nth % 2)) filter-data0)
                                                             filter-data2-nasdaq (filter #(and (>= (nth % 2) price-cutoff) (>= (nth % 3) mc-cutoff)) filter-data1)]
                                                         filter-data2-nasdaq))

    (def nasdaq-data (retrieve-symbols mc-cutoff price-cutoff adv-cutoff-millions (first urls)))
    (def nyse-data (retrieve-symbols mc-cutoff price-cutoff adv-cutoff-millions (second urls)))
    (def tickers (set (concat (map first nyse-data)  (map first nasdaq-data))))
    (count tickers)
    (def stock-names-nyse (zipmap (map first nyse-data)  (map second nyse-data)))
    (def stock-names-nasdaq (zipmap (map first nasdaq-data)  (map second nasdaq-data)))
    (def stock-names (merge stock-names-nyse stock-names-nasdaq))
    (count stock-names))

(def data (io2/read-dataset (str utils/HOME "/data/datagen-grab.txt") :header true))

(utils/lookahead 5 (incanter$/$ :price data))

(defn fsg [k y]
  (let [s (/ (stats/sd (filter #(not (Double/isNaN %)) y)) k)
        out (map #(cond (> (incanter/abs %) s) (utils/sign %) :else 0) y)]
    out))

(defn fsg-number [s y]
  (let [out (map #(cond (> (incanter/abs %) s) (utils/sign %) :else 0) y)]
    out))

(def y (let [p (incanter$/$ :price data)
             fp (utils/lookahead 10 (incanter$/$ :price data))
             y (map - fp p)
             y-fsg (fsg-number 0.50 y)]
         y-fsg))

;(pprint (filter #(not (Double/isNaN %)) y))

(stats/mean (map incanter/abs (filter #(not (Double/isNaN %)) y)))

(incanter/save (incanter/conj-cols (incanter/sel data :except-cols :price) y) (str HOME "/data/trading-fsg.tsv"))
