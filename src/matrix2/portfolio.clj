(ns matrix.portfolio

  (:require
   [matrix.utils :as utils]
   [matrix.data :as data]
   [matrix.indicators :as indicators]
   [matrix.agents :as agents]
   [matrix.tickers :as tickers]
   [matrix.jutsudata :as jutsudata]
   [matrix.tradinglogic :as tradinglogic]
   [repltrader.execution :as execution]
   [repltrader.gateway :as gateway]

   [jutsu.core :as j]
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

   [clojure.tools.cli :refer [cli]] [matrix.utils :as u]

   [taoensso.timbre :as timbre]
   [clojure.term.colors :refer :all])  (:use clojure.pprint)
  (:require [clojure.tools.cli :refer [parse-opts]])

;  (:gen-class)
)

;; work for pre market stuff

(defn getTrades-extended-new-premarket
  [portfolio date]
  (let [;p-folio ["HOG" "TGT"]
       ;date 20180125
        times [["063000" "070000"]
               ["070000" "073000"]
               ["073000" "080000"]
               ["080000" "083000"]
               ["083000" "090000"]
               ["090000" "093000"]]
        dataset (atom '())]
    (doseq [tic portfolio] (doseq [time times]

                             (let [msgs-dirty (data/getTradesNew tic date (first time) (second time))

                                   bind? (cond (= 2 (count (str/split (first msgs-dirty) #",")))  false  :else true)]
                               (if bind? (swap! dataset concat
                                                (data/getTradesNew tic date (first time) (second time)))))))
    (sort-by #(read-string (second (str/split % #","))) @dataset)))

;(take 10 (filter #(= \T (first %)) (getTradesQuotes-extended-new-premarket ["MDLZ"] 20180202 )) )


(defn pre-market-volume [ticker date]
  (reduce + (map #(read-string (nth (str/split % #",") 3)) (getTrades-extended-new-premarket [ticker] date))))

(defn pre-market-price [ticker date]
  (stats/mean (map #(read-string (nth (str/split % #",") 2)) (getTrades-extended-new-premarket [ticker] date))))

(defn pre-market-price-volume [ticker date]
  (let [tmp (getTrades-extended-new-premarket [ticker] date)
        volumes    (map #(read-string (nth (str/split % #",") 3)) tmp)
        prices    (map #(read-string (nth (str/split % #",") 2)) tmp)]

    [(stats/mean prices)  (reduce + volumes)]))

(defn pre-volume-ratio [tic date] (let [;tic "IIVI"
                                ;date 20180202
                                        prev-day (nth (tickers/recent-dates) (- (.indexOf (tickers/recent-dates) date) 1))
                                        todays-volume (pre-market-volume tic date)
                                        yest-volume (pre-market-volume tic prev-day)
                                        ratio (cond (zero? yest-volume) 0 :else (int (/ todays-volume yest-volume)))]
                                    ratio))

(defn pre-price-ratio [tic date] (let [;tic "IIVI"
                                ;date 20180202
                                ;prev-day (nth (tickers/recent-dates) (- (.indexOf (tickers/recent-dates) date) 1 ))
                                       todays-price (pre-market-price tic date)
                                       yest-price (last (:c (tickers/ohlcv-date date tic)))
                                       ratio (cond (zero? yest-price) 0 :else

                                                   (utils/round (apply max [(/ todays-price yest-price) (/ yest-price todays-price)])))]
                                   ratio))

(def results-v
  (let [gt
        (filter tickers/good-ticker? tickers/tickers)] (map vector gt (map #(pre-volume-ratio % 20180202)
                                                                           gt))))

(defn results-p [date]
  (let [gt
        (filter tickers/good-ticker? tickers/tickers)] (map vector gt (map #(pre-price-ratio % date)
                                                                           gt))))

;(pre-price-ratio "AMBA" 20180209)

;(pprint (reverse (sort-by second results-v)))
;(pprint (take 20 (reverse (sort-by second (results-p 20180209)))))

;(def s-v (map vector (map first results-v) (utils/zscore (map second results-v) )))
(;def s-p (map vector (map first results-p) (utils/zscore (map second results-p) ))
)

(;def trade-these (map first (reverse (sort-by second (map vector (map first s-v) (map #(+ (second %2) (second %1 ))  s-v s-p  )))))
)
;(pprint (take 20 (map first (reverse (sort-by second results)))))
;(pprint (str/join "," trade-these))

(def r (results-p 20180209))
(pprint r)
(pprint (str/join "," (map first (take-last  20 (sort-by second r)))))
;(def movers )
;("DECK" "RGC" "EUFN" "ES" "DATA" "AWK" "GRUB" "ADP" "HPQ" "TEAM")


(defn pre-price-ratio-live [indicator-state tic date] (let [;tic "IIVI"

                                                            todays-price (:data (indicators/l1-price indicator-state (keyword tic) (keyword (str date))))
                                                            yest-price (last (:c (tickers/ohlcv-date date tic)))
                                                            ratio (cond (or (zero? todays-price) (zero? yest-price)) 0 :else

                                                                        (apply max  [(/ todays-price yest-price) (/ yest-price todays-price)]))]
                                                        ratio))

(defn pre-volume-ratio-live [indicator-state tic date] (let [;tic "IIVI"

                                                             todays-volume (:data (indicators/l1-volume indicator-state (keyword tic) (keyword (str date))))
                                                             yest-volume (last (:v (tickers/ohlcv-date date tic)))
                                                             ratio (cond (or (zero? todays-volume) (zero? yest-volume)) 0 :else

                                                                         (apply max  [(/ todays-volume yest-volume) (/ yest-volume todays-volume)]))]
                                                         ratio))

;FDC,BAH,BZUN,CTLT,GRUB,DKS,FSLR,GOLD,ES,AOS,EC,HCC,BG,CORT,ARNC,BMY,HES,FCAU,AMD,CZR


(def gt (filter tickers/good-ticker? tickers/tickers))

(defn hl [date aticker] (let [dates (tickers/recent-dates)
                              N (+ 1 (.indexOf dates date))
                              date-next (nth dates N)
                              d (tickers/ohlcv-date date aticker)
                              c (last (:c d))
                              o (last (:o d))
                              hl (incanter/abs (last (map utils/return-bps (:c d) (:o d))))]
;date-next
                          hl
                          [c o]))

;;debug this
;(hl 20180201 "RH")
(def token (utils/random-word))
(io/delete-file "/home/puru/data/tickers.data" :quiet)

(comment [date (drop-last (tickers/recent-dates))]
  ;(def date (nth (tickers/recent-dates ) 21))

         (def hls (map (partial hl date) gt))
         (def movers (map first (take-last 5 (sort-by second (map vector gt hls)))))

;(spit "/home/puru/data/tickers.data" (str(str/join "," movers) " " date " " date " " token "\n" )  :append true )
)
