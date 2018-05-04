(ns matrix.data (:require
                 [matrix.utils :as utils]
                 [matrix.indicators :as indicators]
                 [matrix.agents :as agents]
                 [matrix.timekeeper :as timekeeper]

                 [matrix.tickers :as tickers]
                 [matrix.jutsudata :as jutsudata]
                 [matrix.tradinglogic :as tradinglogic]
                 [matrix.pnl :as pnl]
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

    (:gen-class))

;;slow entry in morning trading seems bad, you get in late and lose
;; elimintate 5 minute check
;; add returns frm open check, dont enter opposite this sign
;; cutoff max loss at 200/300 a tic
;; exit if ro is opposite sign now


;; can talk about good exit and good entry functions
;; good exit functions switch slowly
;; good entry conditions are fast

;; enter on p>p5 and dispersion in inc ; and ro is positive
;; exit if p5<p12 or draw based

;; dont have to take max loss then

;; first reasonable idea is the drawdown implementation in take-position exec logic 20180201
;; next, i want better criteria for entry:
;; use moving mean and SD (say max 5 minutes) to decide whether a point
;; is an activation point or not - leave it to later whether to include activation points going forward or not
;; each signal is activated as above, add market direction signal
;; use majority voting or unanimous to decide entry - backtest

;; bigger question is asset selection from pre-market data
;; this is an accounting question as well - do pre market disclosures cause
;; excess volatility -> are people who file friday night smart?

;; third important question is - should each ticker have its own max loss?
;; small price tickers show lot of volatility - OPGN is example

;; should we use volatility for position sizing?


;; 20180131 in drawdown if bid ask is large, you are already sitting on DD on $1*108 positin, so thats exit - maybe not enter when bask is large?
;; or reference price for m2mpnl compute is mid-price not opposite

;; enter on 3 exit on 4 in cross section? -> does this make money consistently?
;; keep moving window by discarding old data
;; add 52 week high low information to bolster this

;; if spy trend is high, and self trend aligns then enter trade
;; write first relative indicator
;; for that in backtest quotes and trades are sorted


;; fernholz idea needs more work
;; we need clear distributional information on max draw and avg and sharpe
;; based on that we can tune the optimal capital for a ticker
;; practically you have only 40K max
;; ideally you have to run as many tickers as possible
;; next experiment with conditioning on relative volatility - if too high
;; dont trade or reduce risk considerably
;; time of day might be a natural salve for this
;; also solve the implementation shortfall issue
;; does hedging with the market help?

;; right now we peg to open, what if we dynamically peg to a moving
;; average, does that work better?
;; also when dispersion is moving, that is a bad time to peg


;; cross section idea: if we can catch NKTR as its volume explodes, we can make $$
;; first show me a measure of relative volume on a day -> collect 20 day average of per minute volume
;; per minute volume is just ohlcv volume divided by 390 -> approximate -> done
;; write indicator to show how frequently someone is trading it
;; measure log?inverse(t(last-trade) - t(trade-prior-to-that))


;; another idea, if volume profile is average, then do MR else trend follow
;; decide volume profile with z score of cumulative volumes ie high volume day or low?

;; color code your output logs for readability


;; another thread that might be worthwhile
;; market making idea of putting two orders across price
;; make small profit for each ticker and run
;; avoid sharply trending tickers
;; mutreja used a combination of volatility adjusted market making momentum and reversal

;; phase 2 of project
;; need to generate gen strat type of
;; split into ranking exec
;; then simulate ranking
;; all on aws


;; next define indicators using only standardize information
;; convert p-ema into pseudo-price ema
;; write returns-quality

;; be able to run param permute
;; small pnl targets are a good idea
;; write returns quality indicator, based on return stream sharpe ratio
;; should be robust to jumps outliers
;; enter only if returns quality is high in the recent past

;; 20171121, main things to be done:
;; 1. create sim-strategy to test out of sample effectiveness
;; sim-strategy ticker-coll date-coll
;; memoize the tick data functions to test 100 strats per ticker
;; show me results of walkforward for various strats
;; write up volume information


;; portfoflio setup


;(ticker->sector "FOLD")
;; setup


;; read tickers from offline file
(comment (def tickers (sort (concat ["TGT" "TEVA" "HAS"] (take 300 (read-string (slurp "/home/puru/Dropbox/data/small-tickers.data")))))))

(def N 3)

;(def tickers (take N (sort (take 3000 (read-string (slurp "/home/puru/Dropbox/data/most-liquid-tickers.data" ))))))


;(def ticker-date-list (drop 1 (read-string (slurp "/home/puru/Dropbox/data/trade-these-mid.data" ))))


;(def tickers ["MATW"] )
;;these are not used just placeholders
(def portfolio ["NOK" "MDLZ" "BEN" "BABA" "FB" "TWTR" "RIG" "PYPL" "HAL" "VZ"])
(def portfolio ["MDXG"])
                                        ;(pprint ticker-date-list)
;
;(def tickers (concat (second (last (drop-last 2 ticker-date-list))) ["XIV"]  )  )
;(pprint tickers)
(def dates [20171114])
;(def dates (take N (read-string (slurp "/home/puru/Dropbox/data/recent-dates.data"))))

;(def dates [  (first (last (drop-last 2 ticker-date-list)))        ])


;(count dates)
;(pprint dates)

;; important what is eond of day, 3 55 usually


;;eod at 1255 PM
;(def end-time (utils/msec "20171124125500000" ))
;;eod at 3 55 PM
;(def end-time (utils/msec "20171124155500000" ))
;(pprint end-time)
;(utils/tradingTime "201711241555 00000" )
(def end-time-string "20171124155900000")
(def end-time (utils/msec end-time-string))
(pprint "end trading at .. ")
(pprint end-time-string)
;(utils/tradingTime "20171124155500000" )


;; update only so many times
;(def bardata-frequency-msecs 15000)
(def bardata-frequency-msecs (* 1 60 1000))  ;;

;;this is the indicator structure for the stock

;; for ticker and date, this is what is updated on the fly
(defn indicator-base-unit []
  (atom {:price (atom 0.0)
         :midprice (atom 0.0)
         :bidprice (atom 0.0)
         :askprice (atom 0.0)
         :price-bars (atom [])
         :volume-bars (atom [])
         :volume-ratio-bars (atom [])
         :open (atom 0.0)
         :hi (atom 0.0)
         :volume (atom 0.0)
         :buyvolume (atom 0.0)
         :sellvolume (atom 0.0)

         :lo (atom 10000.0)
         :time (atom 0.0)
         :time-string (atom "0.0")
         :previous-trade-time (atom 0.0)
         :trade-time-lapse (atom 0.0)

         :time-bars (atom [])
            ;; range of stock
         :dispersion (atom 0.0)
         :dispersion-bars (atom [])
         :eod? (atom false)
         :eod-plus-one? (atom false)
         :seq-no (atom 0.0) ;; increment for bar data
         :pnl (atom 0.0) ;; m2m pnl for tic and date
}))

;;dates loop
(defn indicator-state-intermediate [dates indicator-base-unit]
  (zipmap (map keyword (map str dates)) (repeatedly (count dates)
                                                    indicator-base-unit)))

;; just loop over tickers
(defn create-indicator-state
  "return indicator instances on tickers and dates, typically assigned to indicator-state variable"
  [tickers dates] (zipmap (map keyword tickers)
                          (repeatedly (count tickers) #(indicator-state-intermediate dates indicator-base-unit))))

;(indicator-state ["TGT" "TEVA"] [20171116 20171117])


;(declare indicator-state)


;; void function now delete


;(my-indicators-reset! ["TGT" "TEVA"] [20171116 20171117 ])

;(indicator-state-intermediate [20171116 20171117 ] indicator-base-unit)
;(def indicator-state (create-indicator-state ["TGT" "TEVA"] dates) )


;; hmmm this is right format; do it again!!!
;; check if this works


;; some helper functions

(do

  (defn getTradesQuotes
    [tic date start end]
    (let [tail (str "&trades=1&quotes=1&beginTime=" date start "&endTime=" date end)]
      (:body (client/get (str "http://localhost:5000/tickData?symbol=" tic tail)))))

  (defn getTrades
    [tic date start end]
    (let [tail (str "&trades=1&quotes=0&beginTime=" date start "&endTime=" date end)]
      (:body (client/get (str "http://localhost:5000/tickData?symbol=" tic tail)))))

  (defn getTradesQuotes-extended
    [tic date]
    (let [times [["093000" "103000"]
                 ["103000" "113000"]
                 ["113000" "123000"]
                 ["123000" "133000"]
                 ["133000" "143000"]
                 ["143000" "153000"]
                 ["153000" "160000"]]
          dataset (atom "")]
      (doseq [time times]

        (swap! dataset str (getTradesQuotes tic date (first time) (second time))))

      (identity @dataset)))

;(getTradesQuotes-extended "BKD" 20180222 )


  (defn getTrades-extended
    [tic date]
    (let [times [["093000" "113000"]
                 ["113000" "133000"]
                 ["133000" "153000"]
                 ["153000" "160000"]]
          dataset (atom "")]
      (doseq [time times]

        (swap! dataset str (getTrades tic date (first time) (second time))))

      (identity @dataset))) (defn getTradesQuotesNew
                              [tic date start end]
                              (let [tail (str "&trades=1&quotes=1&beginTime=" date start "&endTime=" date end)
                                    msgs (:body (client/get (str "http://localhost:5000/tickData?symbol=" tic tail)))
                                    split-msgs (str/split msgs #"\r\n")
          ;out (first split-msgs)
                                    see (map #(conj (str/split % #",") tic)   split-msgs)
                                    see2 (map #(str/join "," %) see)]
      ;out
                                see2))

  (defn getTradesNew
    [tic date start end]
    (let [tail (str "&trades=1&quotes=0&beginTime=" date start "&endTime=" date end)
          msgs (:body (client/get (str "http://localhost:5000/tickData?symbol=" tic tail)))
          split-msgs (str/split msgs #"\r\n")
          ;out (first split-msgs)
          see (map #(conj (str/split % #",") tic)   split-msgs)
          see2 (map #(str/join "," %) see)]
      ;out
      see2))

  (defn getTradesQuotes-extended-new
    [portfolio date]
    (let [;p-folio ["HOG" "TGT"]
       ;date 20180125
          times [["093000" "100000"]
                 ["100000" "103000"]
                 ["103000" "110000"]
                 ["110000" "113000"]
                 ["113000" "120000"]
                 ["120000" "123000"]
                 ["123000" "130000"]
                 ["130000" "133000"]
                 ["133000" "140000"]
                 ["140000" "143000"]
                 ["143000" "150000"]
                 ["150000" "153000"]
                 ["153000" "160000"]]
          dataset (atom '())]
      (doseq [tic portfolio] (doseq [time times]
                               (swap! dataset concat (getTradesQuotesNew tic date (first time) (second time)))))
      (sort-by #(read-string (second (str/split % #","))) @dataset))))

;(take-last 10 (getTradesQuotes-extended-new ["MDP"] 20180202))


;;eod at 1255 PM
;(def end-time (utils/msec "20171124125500000" ))
;;eod at 3 55 PM
;(def end-time (utils/msec "20171124155500000" ))

;; key function that update the indicator state object as data flows in
;; the body will change slightly for real data, this is for backtest data
(defn parse-stream-trade-backtest
  "update all crucial data and bars on each trade tick for selected tickers"
  [indicator-state asset date-numeric msg]
  (let [[_ time_ price0 volume0 _ _ _ _ _ ticker] (str/split msg #",")

        price (Float/parseFloat price0)
        vol (Float/parseFloat volume0)
        k-asset (keyword asset)
        thistime (utils/msec time_)
        date (keyword (str date-numeric))
        previous-time
        (let [t @(:time-bars @((indicator-state k-asset) date))]
          (cond (empty? t) 0 :else (last t)))
        time-lapse (- thistime previous-time)
        hi-price @(:hi @((indicator-state k-asset) date))
        lo-price @(:lo @((indicator-state k-asset) date))
        my-price @(:price @((indicator-state k-asset) date))
        bidprice @(:bidprice @((indicator-state k-asset) date))
        askprice @(:askprice @((indicator-state k-asset) date))
        mid-price (/ (+ bidprice askprice) 2)
        my-volume @(:volume @((indicator-state k-asset) date))

        dispersion (cond (zero? my-price) 0.0 :else (utils/return-bps hi-price lo-price))
        ;current-trade-time @(:time @((indicator-state k-asset) date))
        ;previous-trade-time @(:previous-trade-time @((indicator-state k-asset) date))
        ;trade-time-lapse (- current-trade-time previous-trade-time)
]
;(println msg)
    (when (<= thistime end-time)
      (do (reset! (@((indicator-state k-asset) date) :price)  price)
    ;(reset! (@((indicator-state k-asset) date) :previous-trade-time)  current-trade-time)

          (reset! (@((indicator-state k-asset) date) :time)  thistime)
          (reset! (@((indicator-state k-asset) date) :time-string)  time_)
          (reset! (@((indicator-state k-asset) date) :dispersion)  dispersion)
          (swap! (@((indicator-state k-asset) date) :volume)  + vol)
          (cond
          ;(> price mid-price ) (swap! (@((indicator-state k-asset) date) :buyvolume)  + vol )
            (< price mid-price) (swap! (@((indicator-state k-asset) date) :sellvolume)  + vol)
            (> price mid-price) (swap! (@((indicator-state k-asset) date) :buyvolume)  + vol))
        ;(reset! (@((indicator-state k-asset) date) :trade-time-lapse)  trade-time-lapse )
)

      (if (zero? my-price)

        (do (reset! (@((indicator-state k-asset) date) :open)  price)
            (reset! (@((indicator-state k-asset) date) :hi)  price)
            (reset! (@((indicator-state k-asset) date) :lo)  price)))
      (if (> price hi-price)
        (reset! (@((indicator-state k-asset) date) :hi)  price))

      (if (< price lo-price)
        (reset! (@((indicator-state k-asset) date) :lo)  price))

      (when (>= time-lapse bardata-frequency-msecs)
        (do
          (swap! (@((indicator-state k-asset) date) :price-bars)  conj price)
          (swap! (@((indicator-state k-asset) date) :time-bars)  conj thistime)
          (swap! (@((indicator-state k-asset) date) :dispersion-bars)  conj dispersion)
          (swap! (@((indicator-state k-asset) date) :volume-bars)  conj my-volume)
          (reset! (@((indicator-state k-asset) date) :volume) 0.0)
          (reset! (@((indicator-state k-asset) date) :buyvolume) 0.0)
          (reset! (@((indicator-state k-asset) date) :sellvolume) 0.0)
          (swap! (@((indicator-state k-asset) date) :seq-no) inc)
;(timbre/debug  "seq no update from trade" @(@((indicator-state k-asset) date) :seq-no) )
)))))

;; now write the bid ask monitor

(defn parse-stream-quote-backtest
  "update all crucial data and bars on each trade tick for selected tickers"
  [indicator-state asset date-numeric msg]
  (let
   [[_ time_ bidprice0 askprice0 bidsize0 asksize0 _ _ _] (str/split msg #",")
              ;asset2 (clojure.string/replace asset #":" "")
    bidprice (Float/parseFloat bidprice0)
    askprice (Float/parseFloat askprice0)
    bidsize (Float/parseFloat bidsize0)
    asksize (Float/parseFloat asksize0)
    price  (/ (+ bidprice askprice) 2)
    thistime (utils/msec time_)
    thistime2 (utils/tradingTime time_)
    k-asset (keyword asset)
    date (keyword (str date-numeric))
    previous-time
    (let [t @(:time-bars @((indicator-state k-asset) date))]
      (cond (empty? t) 0 :else (last t)))
    time-lapse (- thistime previous-time)]
    (when (<= thistime end-time) (do
                                   (reset! (@((indicator-state k-asset) date) :bidprice)  bidprice)
                                   (reset! (@((indicator-state k-asset) date) :askprice)  askprice)
                                   (reset! (@((indicator-state k-asset) date) :midprice)  price)
               ;(reset! (@((indicator-state k-asset) date) :price)  price )
)
          (when (>= time-lapse bardata-frequency-msecs)
            (do
              (swap! (@((indicator-state k-asset) date) :price-bars)  conj price)
              (swap! (@((indicator-state k-asset) date) :time-bars)  conj thistime)
              (reset! (@((indicator-state k-asset) date) :volume) 0.0)
              (swap! (@((indicator-state k-asset) date) :seq-no) inc)
              (swap! (@((indicator-state k-asset) date) :volume-bars)  conj 0)
;(timbre/debug  "seq no update from quote" @(@((indicator-state k-asset) date) :seq-no) )
)))))

(defn parse-stream-trade-backtest-new
  "update all crucial data and bars on each trade tick for selected tickers"
  [indicator-state date-numeric msg]
  (let [[_ time_ price0 volume0 _ _ _ _ _ ticker] (str/split msg #",")

        price (Float/parseFloat price0)
        vol (Float/parseFloat volume0)
        k-asset (keyword ticker)
        thistime (utils/msec time_)
        date (keyword (str date-numeric))
        previous-time
        (let [t @(:time-bars @((indicator-state k-asset) date))]
          (cond (empty? t) 0 :else (last t)))
        time-lapse (- thistime previous-time)
        hi-price @(:hi @((indicator-state k-asset) date))
        lo-price @(:lo @((indicator-state k-asset) date))
        my-price @(:price @((indicator-state k-asset) date))
        bidprice @(:bidprice @((indicator-state k-asset) date))
        askprice @(:askprice @((indicator-state k-asset) date))
        mid-price (/ (+ bidprice askprice) 2)

        my-volume @(:volume @((indicator-state k-asset) date))
        bv @(:buyvolume @((indicator-state k-asset) date))
        sv @(:sellvolume @((indicator-state k-asset) date))
        my-volume-ratio (cond (zero? (+ bv sv)) 0 :else (/ bv (+ bv sv)))

        dispersion (cond (zero? my-price) 0.0 :else (utils/return-bps hi-price lo-price))
        ;current-trade-time @(:time @((indicator-state k-asset) date))
        ;previous-trade-time @(:previous-trade-time @((indicator-state k-asset) date))
        ;trade-time-lapse (- current-trade-time previous-trade-time)
]
;(println msg)
    (when (<= thistime end-time)
      (do (reset! (@((indicator-state k-asset) date) :price)  price)
          (reset! (@((indicator-state k-asset) date) :midprice)  price)

    ;(reset! (@((indicator-state k-asset) date) :previous-trade-time)  current-trade-time)

          (reset! (@((indicator-state k-asset) date) :time)  thistime)
          (reset! (@((indicator-state k-asset) date) :time-string)  time_)
          (reset! (@((indicator-state k-asset) date) :dispersion)  dispersion)
          (swap! (@((indicator-state k-asset) date) :volume)  + vol)
          (cond
            (> price mid-price) (swap! (@((indicator-state k-asset) date) :buyvolume)  + vol)
            (< price mid-price) (swap! (@((indicator-state k-asset) date) :sellvolume)  + vol))
        ;(reset! (@((indicator-state k-asset) date) :trade-time-lapse)  trade-time-lapse )
          (reset! timekeeper/global-time-now  thistime))

      (if (zero? my-price)

        (do (reset! (@((indicator-state k-asset) date) :open)  price)
            (reset! (@((indicator-state k-asset) date) :hi)  price)
            (reset! (@((indicator-state k-asset) date) :lo)  price)))
      (if (> price hi-price)
        (reset! (@((indicator-state k-asset) date) :hi)  price))

      (if (< price lo-price)
        (reset! (@((indicator-state k-asset) date) :lo)  price))

      (when (>= time-lapse bardata-frequency-msecs)
        (do
          (swap! (@((indicator-state k-asset) date) :price-bars)  conj price)
          (swap! (@((indicator-state k-asset) date) :time-bars)  conj thistime)
          (swap! (@((indicator-state k-asset) date) :dispersion-bars)  conj dispersion)
          (swap! (@((indicator-state k-asset) date) :volume-bars)  conj my-volume)
          (swap! (@((indicator-state k-asset) date) :volume-ratio-bars)  conj my-volume-ratio)
          (reset! (@((indicator-state k-asset) date) :volume) 0.0)
          (reset! (@((indicator-state k-asset) date) :buyvolume) 0.0)
          (reset! (@((indicator-state k-asset) date) :sellvolume) 0.0)
          (swap! (@((indicator-state k-asset) date) :seq-no) inc)
;(timbre/debug  "seq no update from trade " ticker " " @(@((indicator-state k-asset) date) :seq-no) )
)))))

;(def msgs (getTradesQuotes-extended-new (take 2 portfolio) 20171114))
;(pprint (take 10 msgs))
;(first msgs)
;(parse-stream-quote-backtest-new i-state 20171114 (first msgs))
;(parse-stream-trade-backtest-new i-state 20171114 (nth msgs 5 ))
;(pprint i-state)


;; now write the bid ask monitor

(defn parse-stream-quote-backtest-new
  "update all crucial data and bars on each trade tick for selected tickers"
  [indicator-state date-numeric msg]
  (let
   [[_ time_ bidprice0 askprice0 bidsize0 asksize0 _ _ _ ticker] (str/split msg #",")
              ;asset2 (clojure.string/replace asset #":" "")
    bidprice (Float/parseFloat bidprice0)
    askprice (Float/parseFloat askprice0)
              ;bidsize (Float/parseFloat bidsize0)
              ;asksize (Float/parseFloat asksize0)
    price  (/ (+ bidprice askprice) 2)
    thistime (utils/msec time_)
    thistime2 (utils/tradingTime time_)
    k-asset (keyword ticker)
    date (keyword (str date-numeric))
    previous-time
    (let [t @(:time-bars @((indicator-state k-asset) date))]
      (cond (empty? t) 0 :else (last t)))
    time-lapse (- thistime previous-time)]
    (when (<= thistime end-time) (do
                                   (reset! (@((indicator-state k-asset) date) :bidprice)  bidprice)
                                   (reset! (@((indicator-state k-asset) date) :askprice)  askprice)
                                   (reset! (@((indicator-state k-asset) date) :midprice)  price)
                                        ;(reset! (@((indicator-state k-asset) date) :price)  price )
                                   (reset! timekeeper/global-time-now  thistime))
          (when (>= time-lapse bardata-frequency-msecs)
            (do
              (swap! (@((indicator-state k-asset) date) :price-bars)  conj price)
              (swap! (@((indicator-state k-asset) date) :time-bars)  conj thistime)
              (reset! (@((indicator-state k-asset) date) :volume) 0.0)
              (swap! (@((indicator-state k-asset) date) :seq-no) inc)
              (swap! (@((indicator-state k-asset) date) :volume-bars)  conj 0)
;(timbre/debug  "seq no update from quote" @(@((indicator-state k-asset) date) :seq-no) )
)))))

;; key function that update the indicator state object as data flows in
;; this is for real data
(defn parse-stream-trade
  "update all crucial data and bars on each trade tick for selected tickers"
  [indicator-state date-numeric msg]
  (let [;[_ time_ price0 volume0 _ _ _ _ _ ticker] (str/split msg #",")
        [_ asset _ _ _ _ _ _ price0 volume0 time_] (str/split msg #",")

        price (Float/parseFloat price0)
        vol (Float/parseFloat volume0)
        k-asset (keyword asset)
        thistime (utils/msec time_)
        date (keyword (str date-numeric))
        previous-time
        (let [t @(:time-bars @((indicator-state k-asset) date))]
          (cond (empty? t) 0 :else (last t)))
        time-lapse (- thistime previous-time)
        hi-price @(:hi @((indicator-state k-asset) date))
        lo-price @(:lo @((indicator-state k-asset) date))
        my-price @(:price @((indicator-state k-asset) date))
        my-volume @(:volume @((indicator-state k-asset) date))
        ;current-trade-time @(:time @((indicator-state k-asset) date))
        ;previous-trade-time @(:previous-trade-time @((indicator-state k-asset) date))
        ;trade-time-lapse (- current-trade-time previous-trade-time)
        dispersion (cond (zero? my-price) 0.0 :else (utils/return-bps hi-price lo-price))]
;(println msg)
    (when (<= thistime end-time)
      (do (reset! (@((indicator-state k-asset) date) :price)  price)
;(reset! (@((indicator-state k-asset) date) :previous-trade-time)  current-trade-time)
          (reset! (@((indicator-state k-asset) date) :time)  thistime)
          (reset! timekeeper/global-time-now  thistime)

          (reset! (@((indicator-state k-asset) date) :time-string)  time_)
          (reset! (@((indicator-state k-asset) date) :dispersion)  dispersion)
          (swap! (@((indicator-state k-asset) date) :volume)  + vol)
        ;(reset! (@((indicator-state k-asset) date) :trade-time-lapse)  trade-time-lapse )
)

      (if (zero? my-price)

        (do (reset! (@((indicator-state k-asset) date) :open)  price)
            (reset! (@((indicator-state k-asset) date) :hi)  price)
            (reset! (@((indicator-state k-asset) date) :lo)  price)))
      (if (> price hi-price)
        (reset! (@((indicator-state k-asset) date) :hi)  price))

      (if (< price lo-price)
        (reset! (@((indicator-state k-asset) date) :lo)  price))

      (when (>= time-lapse bardata-frequency-msecs)
        (do
          (swap! (@((indicator-state k-asset) date) :price-bars)  conj price)
          (swap! (@((indicator-state k-asset) date) :time-bars)  conj thistime)
          (swap! (@((indicator-state k-asset) date) :dispersion-bars)  conj dispersion)
          (swap! (@((indicator-state k-asset) date) :volume-bars)  conj my-volume)
          (reset! (@((indicator-state k-asset) date) :volume) 0.0)
          (swap! (@((indicator-state k-asset) date) :seq-no) inc))))))

;;test


;; now write the bid ask monitor for real data

(defn parse-stream-quote
  "update all crucial data and bars on each trade tick for selected tickers"
  [indicator-state date-numeric msg]
  (let
   [;[_ time_ bidprice0 askprice0 bidsize0 asksize0 _ _ _ ] (str/split msg #",")
    [_ asset _ _ _ bidprice0 askprice0 bidsize0 asksize0 time_] (str/split msg #",")
              ;asset2 (clojure.string/replace asset #":" "")
    bidprice (Float/parseFloat bidprice0)
    askprice (Float/parseFloat askprice0)
    bidsize (Float/parseFloat bidsize0)
    asksize (Float/parseFloat asksize0)
    price  (/ (+ bidprice askprice) 2)
    thistime (utils/msec time_)
    thistime2 (utils/tradingTime time_)
    k-asset (keyword asset)
    date (keyword (str date-numeric))
    previous-time
    (let [t @(:time-bars @((indicator-state k-asset) date))]
      (cond (empty? t) 0 :else (last t)))
    time-lapse (- thistime previous-time)]
    (when (<= thistime end-time) (do
                                   (reset! (@((indicator-state k-asset) date) :bidprice)  bidprice)
                                   (reset! (@((indicator-state k-asset) date) :askprice)  askprice)
                                   (reset! (@((indicator-state k-asset) date) :time-string)  time_) ;; i want to know if my algo is that slow?!
                                        ;(reset! (@((indicator-state k-asset) date) :price)  price ) ;; midprice if no traded price?
                                   (reset! (@((indicator-state k-asset) date) :midprice)  price)
                                   (reset! timekeeper/global-time-now  thistime)
               ;(reset! (@((indicator-state k-asset) date) :price)  price )
)
          (when (>= time-lapse bardata-frequency-msecs)
            (do
              (swap! (@((indicator-state k-asset) date) :price-bars)  conj price)
              (swap! (@((indicator-state k-asset) date) :time-bars)  conj thistime)
              (reset! (@((indicator-state k-asset) date) :volume) 0.0)
              (swap! (@((indicator-state k-asset) date) :seq-no) inc)
              (swap! (@((indicator-state k-asset) date) :volume-bars)  conj 0)
 ;(timbre/debug  "seq no update from quote" @(@((indicator-state k-asset) date) :seq-no) )
)))

    ;; new update price as midprice even no trade has happened
))

;(def msgs (getTradesQuotes-extended-new ["TGT" "HOG"] 20171117)   )
;(last (take 10 msgs))
;(parse-stream-trade-backtest-new 20171117 (last (take 10 msgs))  )


;(spit "/home/puru/matw" (getTradesQuotes-extended "MATW" 20171124))


;; now pass indicator state into this
(defn update-data-quotes
  "run through dates and tickers and update the indicator structure, includes quotes"
  [indicator-state ticker date]
  (let [msgs (str/split (getTradesQuotes-extended ticker date)  #"\r\n")
        ;msgs (str/split (getTrades-extended ticker date)  #"\r\n" )
]
    (doseq [msg msgs]
      (if (= \T (first msg)) (parse-stream-trade-backtest indicator-state ticker date msg)
          (if (= \Q (first msg)) (parse-stream-quote-backtest indicator-state ticker date msg))))
    (reset! (@((indicator-state (keyword ticker)) (keyword (str date))) :eod?)  true)
    (reset! (@((indicator-state (keyword ticker)) (keyword (str date))) :eod-plus-one?)  true)))

(defn update-data-quotes-new
  "run through dates and tickers and update the indicator structure, includes quotes"
  [indicator-state portfolio date]
  (let [msgs (getTradesQuotes-extended-new portfolio date)
        ;msgs (str/split (getTrades-extended ticker date)  #"\r\n" )
]
    (doseq [msg msgs]
      (if (= \Q (first msg)) (parse-stream-quote-backtest-new indicator-state date msg)
          (parse-stream-trade-backtest-new indicator-state date msg)))
    (pprint "Setting EOD flags...")
    (doseq [ticker portfolio]
      (reset! (@((indicator-state (keyword ticker)) (keyword (str date))) :eod?)  true)
      (reset! (@((indicator-state (keyword ticker)) (keyword (str date))) :eod-plus-one?)  true))))

;; print trades to file
(defn print-data-trades
  "run through dates and tickers and update the indicator structure, includes quotes"
  [file ticker date]
  (let [msgs (str/split (getTrades-extended ticker date)  #"\r\n")]
    (doseq [msg msgs]
      (if (= \T (first msg)) (spit file (str msg "\n") :append true)))));(print-data-trades "/home/puru/data/nktr.trades" "NKTR" 20180109)


;; write the aboves live trading counterpart

;; live trading counterpart ends


;;test this
;(update-data "TGT" 20171117)
;; takes time, update now!
;(doall (for [this-ticker tickers this-date dates] (update-data this-ticker this-date)))


;(def tic (keyword (first tickers)) )
;(def date (keyword (str (first dates))))

;; show me price dispersion to check if correct
;(utils/lPlot @(:price-bars @((indicator-state tic) date)))
;(utils/lPlot @(:dispersion-bars @((indicator-state tic) date )))
(comment (doseq [date dates] (utils/lPlot (:dispersion-bars @((indicator-state tic) date)))))

;;; show me returns
;
(comment (utils/lPlot (let [price @(:price-bars @((indicator-state tic) date))]
                        (conj (map utils/return-bps-col (partition 2 1 price)) 0))))

;; with a lookback of 5 units, what is the stdev of returns, ema
(comment (utils/lPlot (let [price @(:price-bars @((indicator-state tic) date))

                            returns (conj (map utils/return-bps-col (partition 2 1 price)) 0)]

                        (utils/ema 20 (map #(utils/sd (take-last 5 (take % returns)))  (range 1 (+ 1 (count  returns))))))))

;; with a lookback of 10 units, what is the sum of returns

(comment (utils/lPlot  (let [price @(:price-bars @((indicator-state tic) date))
                             returns (conj (map utils/return-bps-col (partition 2 1 price)) 0)]

                         (map #(reduce + (take-last 5 (take % returns)))  (range 1 (+ 1 (count returns)))))))

;; ok now line up all these guys together for a predictive model
;; now these are all functions on price-bars, can be done by putting a watch
;; on :price-bars in simulation object
                                        ;(identity tic)

(def indicator-state (create-indicator-state portfolio dates))

;; indicators moved to indicators.clj


(defn eod-flat?
  "returns true if day has ended as per indicator state object"
  [indicator-state tic date]  (let [eod? @(:eod? @((indicator-state tic) date))]
                                eod?))

;; define strat
;; if recent returns > 1% and stdev lo enter
;; exit on pnl target

;; general form of long entry is a function of indicator state,
;; which is a listener to bar-data

;; trade is
;; dispersion > 300 bps
;; delta dispersion is non zero - logic prod is moving in breakout
;; look at ema5-ema12 for direction and enter (might need to calibrate 0 points
;; from past data, ie when is it just noise?
;; use dd exit of 50 bps? with ema 5
;; max otl of 100 bps again with ema 5


;; make forecast using ema diff in the next 5 mins
;; calculate expected pnl
;; if positive expectation after t-cost, enter trade
;; when bps target is reached exit trade

;--> next step
;; working as expected, but entry in diff of ema has to be calibrated
;; appears that high volatility is bad, so we can add conditions around that


;(str (str/join "," [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7)  ]) "\n")


;[current-position pnl m2m-pnl-bps max-pnl-this-round total-pnl ]


;(def token (utils/random-word))
(declare token)

;;write a max-loss map that cuts tickers at a certain loss 100 bps for BPMC seems appropriate from data, for a mean reversion strat
(def max-loss
  {:BPMC 100.0})

;; each agent attaches to each tic date matrix entry
(defn agent-state-intermediate [agent dates]
  (zipmap (map keyword (map str dates)) (repeatedly (count dates)
                                                    agent)))

;; just loop over tickers
(defn create-agent-state [agent tickers dates] (zipmap (map keyword tickers) (repeatedly (count tickers) #(agent-state-intermediate agent dates))))

;;check this works
;(create-agent-state #(agent-2 30 60 100) tickers [20171116 20171117])

;;void now


;; check this
;@((agent-state :TGT) :20171117)


;; token identifies each run for datagen and pnl results reconsiliation
;(def token (utils/random-word))

;;define datagen file
;(declare datagen-file (str utils/HOME "/data/datagen-" (str (gensym token )) ".txt" ))
;(pprint (str "data gen file " datagen-file))

;(declare datagen-file)
(def datagen-file (str utils/HOME "/data/datagen-" (str (gensym token)) ".txt"))

;;trading logic
(defn trading-logic
  "core function, called only when bar data is updated.
to do: otl loss, global max loss ttc cuts larger position
"
  [datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)
        entry-condition (first ((:entry agent) indicator-state tic date))

        datagen-output (cond (nil? (:datagen agent)) nil
                             :else ((:datagen agent) indicator-state tic date)) short-entry-condition (second ((:entry agent) indicator-state tic date))

        ;allowed-to-enter? true

        uts (agent :uts)
        current-position @(agent :position)
        current-n-trades @(agent :n-trades)
        allowed-to-enter? (< current-n-trades (agent :max-n-trades));;correct pnl accounting, if long i can get out on bid only
        reference-price (cond (pos? current-position) bidprice (neg? current-position) askprice :else price)
        mur (agent :mur)
        ;pnl (agent :pnl)
        last-entry @(agent :last-entry-price)
        agent-name (agent :name)

        last-entry-time @(agent :last-entry-time)
        time-now @(@((indicator-state tic) date) :time)
        ttc-now-minutes (cond (zero? current-position) 0.0 :else (utils/round (/ (- time-now last-entry-time) 60000)))
        pnls (cond (empty? last-entry) '(0.0) :else (map utils/round (map #(* % current-position) (map - (repeat (count last-entry) reference-price) last-entry))))
        pnls-bps (cond (empty? last-entry) '(0.0) :else
                       (map utils/round (map
                                         #(* % (utils/sign current-position))  (map utils/return-bps (repeat (count last-entry) reference-price)  last-entry))))

        pnl (utils/round (stats/mean pnls))
        pnl-bps (utils/round (stats/mean pnls-bps))
        max-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps
        total-pnl @(:total-pnl agent) ;; this is in bps
        m2m-pnl-bps @(:m2m-pnl-bps agent) ;; this is in bps

        drawdown (- max-pnl-this-round pnl-bps)
        ;dummy (pprint current-position)
        pnl-target-reached? (cond (nil? (:pnl-target-bps agent)) false :else (>= pnl-bps (:pnl-target-bps agent)))
        drawdown-breached? (cond (nil? (:max-draw-bps agent)) false :else (>= drawdown (:max-draw-bps agent)))
        ttc-breached? (cond (nil? ttc-now-minutes) false :else (>= ttc-now-minutes (:max-ttc-minutes agent))) eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.0
        first-datapoint? (not (.exists (io/as-file datagen-file)))
        agent-datagen-output [agent-name current-position pnl m2m-pnl-bps max-pnl-this-round total-pnl drawdown ttc-now-minutes bidprice askprice (first last-entry)]
        agent-datagen-header ["agent-name" "current-position" "pnl" "m2m-pnl-bps" "max-pnl-this-round" "total-pnl" "drawdown" "ttc" "bidprice" "askprice" "last-entry"]]
    ;;remember eod positions and total-pnls are not correct in datagen bcos
    ;; after eod it does not print another point
    (if datagen? (if first-datapoint?
                   (spit datagen-file (utils/coll->string (concat (:header datagen-output) agent-datagen-header)) :append true)
                   (spit datagen-file (utils/coll->string (concat (:data datagen-output) agent-datagen-output)) :append true)))
    (if (and eod? (not eod-plus-one?))

      (do

        ;(pprint (str "doing eod process " tic date))

        (when (not (zero? current-position))
          (swap! (:position agent) - current-position)
          (swap! (:total-pnl agent) + (- (reduce + pnls) commission))
          (pprint (str "eod pnl " tic " " date " " @(:total-pnl agent))))

          ;; at EOD, the current list of prices is the list of positions we have, so pnl is just sum of these pnls
);;else not eod so do regular stuff
      (cond
        ;true (pprint "I com here")
        (zero? current-position)
        (do (when (and entry-condition allowed-to-enter?)
   ;(pprint "long inc enter...")
              (do
           ;(alter ((sim-results this-agent) date) update-in [:position] + uts )
                (swap! (:position agent) + uts)
                (swap! (:last-entry-price agent) conj askprice)
                (reset! (:last-entry-time agent) time-now))
 ;  (enterlong)
)
            (when (and short-entry-condition allowed-to-enter?)
      ;(pprint "short inc enter...")
              (do
                (swap! (:position agent) - uts)
                (swap! (:last-entry-price agent) conj bidprice)
                (reset! (:last-entry-time agent) time-now))))
        (not (zero? current-position))
        (do (reset! (:m2m-pnl-bps agent) pnl-bps)
            (when (> pnl-bps max-pnl-this-round)
              (reset! (:max-pnl-this-round agent) pnl-bps))
            (when (or pnl-target-reached? drawdown-breached? ttc-breached?) ;; for now only this exit
  ;;time to get out
;(pprint (str "pnl target reached, consider exit at pnl " (first pnls)))
;  (exitposition)
              (cond
                (> current-position 0)
                (do
                  (swap! (:position agent) - uts)
                  (swap! (:last-exit-price agent) conj price)
                  (swap! (:last-entry-price agent) pop)
                  (swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)))
                  (reset! (:m2m-pnl-bps agent) 0.0)
                  (reset! (:max-pnl-this-round agent) 0.0)
                  (reset! (:last-entry-time agent) nil)
                  (swap! (:n-trades agent) inc))
                (< current-position 0)
                (do
                  (swap! (:position agent) + uts)
                  (swap! (:last-exit-price agent) conj price)
                  (swap! (:last-entry-price agent) pop)
                  (swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)))
                  (reset! (:m2m-pnl-bps agent) 0.0)
                  (reset! (:max-pnl-this-round agent) 0.0)
                  (reset! (:last-entry-time agent) nil)
                  (swap! (:n-trades agent) inc)))))
;;update m2m pnl

;; position is at +-mur
);;consider entry
)))

;; trading logic, called for each strat, tic date


;;;;;;; this is a different logic, for market making type of behavior

(defn trading-logic-make-markets
  "core function, called only when bar data is updated.
market making
commit is true means you are ready to commit to a band and wait
this is when volatility is low, can be a fn of indicators
calculate band at all times as price+-dp when commit is false
if commit is true, then commit to band, dont update band, watch prices and enter if true
trade n times
for now i only exit on profit goals, ideally this exit is based on other side of band
"
  [log-file-name datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [dummy (timbre/debug "TL called with " log-file-name datagen? agent indicator-state tic date)
        price @(@((indicator-state tic) date) :price)
        price-bars @(@((indicator-state tic) date) :price-bars)
                                        ;sigma (incanter/sd price-bars)
        sigma (:price-band-cents agent)       ;; only 3 cents for now
        ;price-band [ (utils/round (- price sigma)) (utils/round (+ price sigma)) ]
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)
        commit? (first ((:commit agent) indicator-state tic date)) ;; update band if commit not true, else monitor for entry exit only,
        long? (second ((:commit agent) indicator-state tic date)) ;; update band if commit not true, else monitor for entry exit only,

        short? (nth ((:commit agent) indicator-state tic date) 2) ;; update band if commit not true, else monitor for entry exit only,
;; reversed for momentum
        short-entry-condition (and short? @(:commit-lock agent) (< price @(:price-band-lower agent)))

        entry-condition (and long?  @(:commit-lock agent) (> price @(:price-band-upper agent)));uts (agent :uts)
        capital 1000.0
        uts (:data (indicators/position-to-take capital indicator-state tic date))
        current-position @(agent :position)
        current-position-abs (incanter/abs current-position)
        exit? (cond (pos? current-position) short-entry-condition (neg? current-position) entry-condition :else false)
        current-n-trades @(agent :n-trades)
        allowed-to-enter? (and @(:allowed-to-enter? agent) (< current-n-trades (agent :max-n-trades)))
        ;;correct pnl accounting, if long i can get out on bid only
        reference-price (cond (pos? current-position) bidprice (neg? current-position) askprice :else price)
        mur (agent :mur)
        ;pnl (agent :pnl)
        last-entry @(agent :last-entry-price)
        agent-name (agent :agent-name)

        last-entry-time @(agent :last-entry-time)
        time-now @(@((indicator-state tic) date) :time)
        ttc-now-minutes (cond (zero? current-position) 0.0 :else (utils/round (/ (- time-now last-entry-time) 60000)))
        pnls (cond (empty? last-entry) '(0.0) :else (map utils/round (map #(* % current-position) (map - (repeat (count last-entry) reference-price) last-entry))))
        pnls-bps (cond (empty? last-entry) '(0.0) :else
                       (map utils/round (map
                                         #(* % (utils/sign current-position))  (map utils/return-bps (repeat (count last-entry) reference-price)  last-entry))))

        pnl (utils/round (stats/mean pnls))
        pnl-bps (utils/round (stats/mean pnls-bps))
        max-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps
        total-pnl @(:total-pnl agent) ;; this is in bps
        m2m-pnl-bps @(:m2m-pnl-bps agent) ;; this is in bps

        drawdown (- max-pnl-this-round pnl-bps)
        ;dummy (pprint current-position)
        pnl-target-reached? (cond (nil? (:pnl-target-bps agent)) false :else (>= pnl-bps (:pnl-target-bps agent)))
        drawdown-breached? (cond (nil? (:max-draw-bps agent)) false :else (>= drawdown (:max-draw-bps agent)))
        ttc-breached? (cond (nil? ttc-now-minutes) false :else (>= ttc-now-minutes (:max-ttc-minutes agent))) eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.5
        first-datapoint? (not (.exists (io/as-file datagen-file)))
        agent-datagen-output [agent-name current-position pnl m2m-pnl-bps max-pnl-this-round total-pnl drawdown ttc-now-minutes bidprice askprice (first last-entry)]
        agent-datagen-header ["agent-name" "current-position" "pnl" "m2m-pnl-bps" "max-pnl-this-round" "total-pnl" "drawdown" "ttc" "bidprice" "askprice" "last-entry"]

        datagen-output (cond (nil? (:datagen agent)) nil
                             :else ((:datagen agent) indicator-state tic date))
        live? (cond (nil? @(:live? agent)) false :else @(:live? agent)) ;; place orders
        time @(:seq-no @((indicator-state tic) date))
        ib-tic (name tic)
        cash-account @(:cash-account agent) ;; this is in bps
        jutsu? (cond (nil? (:jutsu agent)) false :else (:jutsu agent))]
    ;;remember eod positions and total-pnls are not correct in datagen bcos
    ;; after eod it does not print another point
    (if jutsu? ;; start server from main

      ;;else update from jutsu of agent
      (doseq [update ((:jutsu agent) indicator-state tic date)
                                        ;dummy (pprint update)
]
        (do
          ;(pprint update)
          (j/update-graph!
           (:chart update)
           (:plotly-data update))
          (Thread/sleep 10)))) (if (and datagen? (:print-datagen? agent)) (if first-datapoint?
                                                                            (spit datagen-file (utils/coll->string (concat (:header datagen-output) agent-datagen-header)) :append true)
                                                                            (spit datagen-file (utils/coll->string (concat (:data datagen-output) agent-datagen-output)) :append true)))
    (if (and eod? (not eod-plus-one?))
      (do

        (when (not (zero? current-position))
          (swap! (:position agent) - current-position)
          (reset! (:eod-position agent) current-position)
          (swap! (:n-trades agent) inc)
       ;(swap! (:total-pnl agent ) + (- (reduce + pnls) commission) )
          (if (pos? current-position) (swap! (:cash-account agent) + (* current-position-abs bidprice)) (swap! (:cash-account agent) - (* current-position-abs askprice)))
          (reset! (:allowed-to-enter? agent) false)
        ;(timbre/debug "EOD... time tic price pnl" time tic price @(:total-pnl agent))
          (if live? (if (pos? current-position)
                      (execution/SELL ib-tic (incanter/abs current-position))
                      (execution/BUY ib-tic (incanter/abs current-position))))

          (reset! (:total-pnl agent) (int (- @(:cash-account agent) (* commission @(:n-trades agent)))))
          (reset! (:m2m-pnl agent) @(:cash-account agent))
;; eod pnl can be max pnl or min
          (let [pnl @(:m2m-pnl agent)]
            (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
            (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl))))

        (spit log-file-name (str (name date) " stock " ib-tic " agent-name " agent-name " n-trades " @(:n-trades agent)  " max-pos " @(:max-position agent)  " min-pos "  @(:min-position agent) " eod position "  @(:eod-position agent)     " max-pnl " (int @(:max-pnl agent)) " min-pnl " (int @(:min-pnl agent))  " net-pnl " (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))        "\n") :append true)

        (let [net-pnl   (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))]
          (if (pos? net-pnl)

            (timbre/debug (green (str (name date) " stock " ib-tic " agent-name " agent-name " n-trades " @(:n-trades agent)  " max-pos " @(:max-position agent)  " min-pos "  @(:min-position agent) " eod position "  @(:eod-position agent)     " max-pnl " (int @(:max-pnl agent)) " min-pnl " (int @(:min-pnl agent))  " net-pnl " (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))        "\n")))

            (timbre/debug (red (str (name date) " stock " ib-tic " agent-name " agent-name " n-trades " @(:n-trades agent)  " max-pos " @(:max-position agent)  " min-pos "  @(:min-position agent) " eod position "  @(:eod-position agent)     " max-pnl " (int @(:max-pnl agent)) " min-pnl " (int @(:min-pnl agent))  " net-pnl " (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))        "\n")))));; at EOD, the current list of prices is the list of positions we have, so pnl is just sum of these pnls
)

      (do
        (if commit? (reset! (:commit-lock agent) true))
        (if (not @(:commit-lock agent))
          (do
            (reset! (:price-band-lower agent)  (utils/round (- price sigma)))
            (reset! (:price-band-upper agent)  (utils/round (+ price sigma)))
 ;(timbre/debug "set band... time tic price-band1 2" time tic @(:price-band-lower agent ) @(:price-band-upper agent )  )
)
;;else commit-lock is true, i have to monitor entry exit
          (do
            (cond

              (zero? current-position)
              (do (when (and entry-condition allowed-to-enter?)
                                        ;(pprint "long inc enter...")
                    (do
                                        ;(alter ((sim-results this-agent) date) update-in [:position] + uts )
                      (swap! (:position agent) + uts)
                      (swap! (:last-entry-price agent) conj askprice)
                      (reset! (:last-entry-time agent) time-now)
             ;(timbre/debug "check band... time tic price-band1 2" time tic @(:price-band-lower agent ) @(:price-band-upper agent )  )
             ;(timbre/debug "Enter long... time tic price pnl" time tic price @(:total-pnl agent))
                      (swap! (:cash-account agent) - (* uts askprice))
            ;(timbre/debug "buy-hedge call" capital indicator-state tic date )
                      (execution/buy-hedge capital indicator-state tic date) (swap! (:n-trades agent) inc))
                                        ;  (enterlong)
)
                  (when (and short-entry-condition allowed-to-enter?)
                                        ;(pprint "short inc enter...")
                    (do
                      (swap! (:position agent) - uts)
                      (swap! (:last-entry-price agent) conj bidprice)
                      (reset! (:last-entry-time agent) time-now)
             ;(timbre/debug "check band... time tic price-band1 2" time tic @(:price-band-lower agent ) @(:price-band-upper agent )  )
             ;(timbre/debug "Enter short... time tic price pnl" time tic price @(:total-pnl agent))
                      (swap! (:cash-account agent) + (* uts bidprice))
            ;(timbre/debug "sell-hedge call" capital indicator-state tic date )
                      (execution/sell-hedge capital indicator-state tic date)
                      (swap! (:n-trades agent) inc))))
              (not (zero? current-position))  ;; update pnls
              (do (reset! (:m2m-pnl-bps agent) pnl-bps)
                  (when (> pnl-bps max-pnl-this-round)
                    (reset! (:max-pnl-this-round agent) pnl-bps))
                  (when (or pnl-target-reached? drawdown-breached? ttc-breached?) ;; change exit idea
          ;;time to get out
                                        ;(pprint (str "pnl target reached, consider exit at pnl " (first pnls)))
                                        ;  (exitposition)
                    (cond
                      (> current-position 0)
                      (do
                        (swap! (:position agent) - uts)
                        (swap! (:last-exit-price agent) conj price)
                        (swap! (:last-entry-price agent) pop)
                        (swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)))
                        (reset! (:m2m-pnl-bps agent) 0.0)
                        (reset! (:max-pnl-this-round agent) 0.0)
                        (reset! (:last-entry-time agent) nil)
                        (swap! (:n-trades agent) inc)
                        (reset! (:commit-lock agent) false)   ;; start updating price bands
                        (swap! (:cash-account agent) + (* current-position-abs bidprice))
                        (execution/sell-hedge capital indicator-state tic date)

             ;(timbre/debug "Exit long... time tic price pnl" time tic price @(:total-pnl agent))
)
                      (< current-position 0)
                      (do
                        (swap! (:position agent) + uts)
                        (swap! (:last-exit-price agent) conj price)
                        (swap! (:last-entry-price agent) pop)
                        (swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)))
                        (reset! (:m2m-pnl-bps agent) 0.0)
                        (reset! (:max-pnl-this-round agent) 0.0)
                        (reset! (:last-entry-time agent) nil)
                        (swap! (:n-trades agent) inc)
                        (reset! (:commit-lock agent) false)
                        (swap! (:cash-account agent) - (* current-position-abs askprice))
                        (execution/buy-hedge capital indicator-state tic date)
             ;(timbre/debug "Exit short... time tic price pnl" time tic price @(:total-pnl agent))
)))))

            (reset! (:m2m-pnl agent) (+ @(:cash-account agent) (* @(:position agent) reference-price)))
            (let [position @(:position agent)]
              (if (> position @(:max-position agent)) (reset! (:max-position agent) position))
              (if (< position @(:min-position agent)) (reset! (:min-position agent) position)))
            (let [pnl @(:m2m-pnl agent)]
              (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
              (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl))) (do (timbre/debug (green "STATUS " " stock " ib-tic " current-position " @(:position agent) " n-trades " @(:n-trades agent) " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " net-pnl " (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))))
                                                                                ((indicator-state tic) date)
                                                                                (reset! (@((indicator-state tic) date) :pnl)   @(:m2m-pnl agent)))))) ;; if commit true, then lock commit-lock into true, stop updating price band
)))

;; trading logic, called for each strat, tic date


;;; market making logic ends

;; open jutsu window before calls from tradng logic
(def first-time? (atom true))
(def running-trace-id (atom 0))

;(j/start-jutsu!)


;;trading logic
(defn trading-logic-follow-trend
  "core function, called only when bar data is updated.
to do: otl loss, global max loss ttc cuts larger position
"
  [datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)
        entry-condition (first ((:entry agent) indicator-state tic date))
        exit-condition (cond (nil? (:exit agent)) false :else (first ((:exit agent) indicator-state tic date))) datagen-output (cond (nil? (:datagen agent)) nil
                                                                                                                                     :else ((:datagen agent) indicator-state tic date))
        jutsu? (if (nil? (:jutsu agent)) false
                   true)

        ;(every? utils/notnan? datagen-output)
        short-entry-condition (second ((:entry agent) indicator-state tic date))
        short-exit-condition (cond (nil? (:exit agent)) false :else (second ((:exit agent) indicator-state tic date))) uts (agent :uts)
        current-position @(agent :position)
        current-n-trades @(agent :n-trades)
        allowed-to-enter? (and
                           @(:allowed-to-enter? agent)
                           (< current-n-trades (agent :max-n-trades)));;correct pnl accounting, if long i can get out on bid only
        reference-price (cond (pos? current-position) bidprice (neg? current-position) askprice :else price)
        mur (agent :mur)
        ;pnl (agent :pnl)
        last-entry @(agent :last-entry-price)
        agent-name (agent :name)

        last-entry-time @(agent :last-entry-time)
        time-now @(@((indicator-state tic) date) :time)
        ttc-now-minutes (cond (zero? current-position) 0.0 :else (utils/round (/ (- time-now last-entry-time) 60000)))
        pnls (cond (empty? last-entry) '(0.0) :else (map utils/round (map #(* % current-position) (map - (repeat (count last-entry) reference-price) last-entry))))
        pnls-bps (cond (empty? last-entry) '(0.0) :else
                       (map utils/round (map
                                         #(* % (utils/sign current-position))  (map utils/return-bps (repeat (count last-entry) reference-price)  last-entry))))

        pnl (utils/round (stats/mean pnls))
        pnl-bps (utils/round (stats/mean pnls-bps))
        max-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps
        min-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps

        total-pnl @(:total-pnl agent) ;; this is in bps
        m2m-pnl-bps @(:m2m-pnl-bps agent) ;; this is in bps

        drawdown (- max-pnl-this-round pnl-bps)
        ;dummy (pprint current-position)
        pnl-target-reached? (cond (nil? (:pnl-target-bps agent)) false :else (>= pnl-bps (:pnl-target-bps agent)))
        drawdown-breached? (cond (nil? (:max-draw-bps agent)) false :else (>= drawdown (:max-draw-bps agent)))
        ttc-breached? (cond (nil? ttc-now-minutes) false :else (>= ttc-now-minutes (:max-ttc-minutes agent)))
        max-loss-breached? (cond (nil? (max-loss tic)) false :else (< pnl-bps (- (max-loss tic)))) eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.0
        datagen-file (str utils/HOME "/data/datagen-" token  ".txt")

        first-datapoint? (not (.exists (io/as-file datagen-file)))
        agent-datagen-output [agent-name current-position pnl m2m-pnl-bps max-pnl-this-round total-pnl drawdown ttc-now-minutes bidprice askprice (first last-entry)]
        agent-datagen-header ["agent-name" "current-position" "pnl" "m2m-pnl-bps" "max-pnl-this-round" "total-pnl" "drawdown" "ttc" "bidprice" "askprice" "last-entry"]
        time @(:seq-no @((indicator-state tic) date))
        ;dummy (log/debug "Trading logic called..")
        live? (cond (nil? @(:live? agent)) false :else @(:live? agent)) ;; place orders
        ib-tic (name tic)
        ;dummy (log/debug "Trading logic called.." live? ib-tic)
];; start  jutsu
    (if jutsu? (if (zero? time)
                 ;; start server plot first point
                 (do (when @first-time? (j/start-jutsu!) (reset! first-time? false))

                 ;(log/debug "waiting for server")
                 ;(Thread/sleep 3000)
                 ;(log/debug "value of jutsu" ((:jutsu agent) indicator-state tic date  ) )
                 ;; assign trace id
                     (swap! jutsudata/trace-id-map assoc-in [tic] @running-trace-id)
                     (swap! running-trace-id inc)
                     (j/graph!
                      "Foo"
                      (first ((:jutsu agent) indicator-state tic date))))
             ;;else update from jutsu of agent
                 (let [update (second ((:jutsu agent) indicator-state tic date))]
                   (j/update-graph!
                    "Foo"
                    update))));;end jutsu
    (if datagen? (if first-datapoint?
                   (spit datagen-file (utils/coll->string (concat (:header datagen-output) ;agent-datagen-header
                                                                  )):append true)
                   (if (every? utils/notnan? (:data datagen-output))
                     (spit datagen-file (utils/coll->string (concat (:data datagen-output) ;agent-datagen-output
                                                                    )):append true)))) (if (and eod? (not eod-plus-one?))

                                                                                         (do
                                                                                           (reset! (:allowed-to-enter? agent) false)
                                                                                           (timbre/debug "doing eod process " time tic date)

                                                                                           (when (not (zero? current-position))
                                                                                             (swap! (:position agent) - current-position)
                                                                                             (swap! (:total-pnl agent) + (- (reduce + pnls) commission))
                                                                                             (if live? (if (pos? current-position)
                                                                                                         (execution/SELL ib-tic (incanter/abs current-position))
                                                                                                         (execution/BUY ib-tic (incanter/abs current-position)))))
                                                                                           (timbre/debug "EOD:  tic date pnl min-pnl-bps n-trades " tic date @(:total-pnl agent) @(:min-pnl-this-round agent) @(:n-trades agent))
          ;; at EOD, the current list of prices is the list of positions we have, so pnl is just sum of these pnls
)
      ;;else not eod so do regular stuff
                                                                                         (cond
        ;true (pprint "I com here")
                                                                                           (zero? current-position)
                                                                                           (do (when (and entry-condition allowed-to-enter?)
                                                                                                 (timbre/debug "long inc enter...time tic price pnl" time tic price @(:total-pnl agent))
                                                                                                 (do
           ;(alter ((sim-results this-agent) date) update-in [:position] + uts )
                                                                                                   (swap! (:position agent) + uts)
                                                                                                   (swap! (:last-entry-price agent) conj askprice)
                                                                                                   (reset! (:last-entry-time agent) time-now)
                                                                                                   (if live? (execution/BUY ib-tic uts)))
 ;  (enterlong)
)
                                                                                               (when (and short-entry-condition allowed-to-enter?)
                                                                                                 (timbre/debug "short inc enter...time tic price pnl" time tic price @(:total-pnl agent))
                                                                                                 (do
                                                                                                   (swap! (:position agent) - uts)
                                                                                                   (swap! (:last-entry-price agent) conj bidprice)
                                                                                                   (reset! (:last-entry-time agent) time-now)
                                                                                                   (if live? (execution/SELL ib-tic uts)))))
                                                                                           (not (zero? current-position))
                                                                                           (do

                                                                                             (reset! (:m2m-pnl-bps agent) pnl-bps)
    ;(log/debug "pnl update...time tic pnl price" time tic pnl price)
                                                                                             (when (> pnl-bps max-pnl-this-round)
                                                                                               (reset! (:max-pnl-this-round agent) pnl-bps))
                                                                                             (when (< pnl-bps min-pnl-this-round)
                                                                                               (reset! (:min-pnl-this-round agent) pnl-bps))

                                                                                             (when (or
                                                                                                    pnl-target-reached? drawdown-breached? ttc-breached? max-loss-breached?
                                                                                                    (and (pos? current-position) exit-condition) (and (neg? current-position) short-exit-condition)) ;; for now only this exit
  ;;time to get out
;(pprint (str "pnl target reached, consider exit at pnl " (first pnls)))
                                        ;  (exitposition)

                                                                                               (cond
                                                                                                 (> current-position 0)
                                                                                                 (do
           ;(log/debug "exiting long position..." time tic)
                                                                                                   (swap! (:position agent) - uts)
                                                                                                   (swap! (:last-exit-price agent) conj price)
                                                                                                   (swap! (:last-entry-price agent) pop)
                                                                                                   (swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)))
                                                                                                   (reset! (:m2m-pnl-bps agent) 0.0)
                                                                                                   (reset! (:max-pnl-this-round agent) 0.0)
                                                                                                   (reset! (:last-entry-time agent) nil)
                                                                                                   (swap! (:n-trades agent) inc)
                                                                                                   (timbre/debug "exiting long position... time tic price pnl" time tic price @(:total-pnl agent))
                                                                                                   (if live? (execution/SELL ib-tic uts)))
                                                                                                 (< current-position 0)
                                                                                                 (do
           ;(log/debug "exiting short position..." time tic)
                                                                                                   (swap! (:position agent) + uts)
                                                                                                   (swap! (:last-exit-price agent) conj price)
                                                                                                   (swap! (:last-entry-price agent) pop)
                                                                                                   (swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)))
                                                                                                   (reset! (:m2m-pnl-bps agent) 0.0)
                                                                                                   (reset! (:max-pnl-this-round agent) 0.0)
                                                                                                   (reset! (:last-entry-time agent) nil)
                                                                                                   (swap! (:n-trades agent) inc)
                                                                                                   (timbre/debug "exiting short position... time tic price pnl" time tic price @(:total-pnl agent))
                                                                                                   (if live? (execution/BUY ib-tic uts))))))
;;update m2m pnl

;; position is at +-mur
);;consider entry
)  ;; eod match
);;remember eod positions and total-pnls are not correct in datagen bcos
    ;; after eod it does not print another point

      ;; eod match
)

;; trading logic, called for each strat, tic date


;; trading logic for fernholz
;; agent ouputs target position
;; logic compares both with threshold in agent, and places order
;; eod same all else is same
;; cash accounting needs to be to manage arbit positions

;;trading logic
(defn trading-logic-fernholz
  "core function, called only when bar data is updated.
to do: otl loss, global max loss ttc cuts larger position
"
  [log-file-name datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)

        current-position @(agent :position)
        current-position-abs (incanter/abs current-position)

        target-position ((:target-position agent) indicator-state tic date)
        ;; if NaN this becomes zero!
        share-diff (utils/round2 0 (- target-position current-position))
        share-diff-abs (incanter/abs share-diff)
        threshold (:inventory-threshold agent)

        cash-account @(:cash-account agent)
        m2m-pnl @(:m2m-pnl agent)

        current-n-trades @(agent :n-trades)
        agent-name (agent :agent-name)

        allowed-to-enter? (and
                           @(:allowed-to-enter? agent)
                           (< current-n-trades (agent :max-n-trades)));;correct pnl accounting, if long i can get out on bid only
        reference-price (cond (pos? current-position) bidprice (neg? current-position) askprice :else price) eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.5 time @(:seq-no @((indicator-state tic) date))
        ;dummy (log/debug "Trading logic called..")
        live? (cond (nil? @(:live? agent)) false :else @(:live? agent)) ;; place orders
        ib-tic (name tic)
        jutsu? (cond (nil? (:jutsu agent)) false :else (:jutsu agent))

        ;dummy (log/debug "Trading logic called.." live? ib-tic)
]

    (if jutsu? ;; start server from main

      ;;else update from jutsu of agent
      (doseq [update ((:jutsu agent) indicator-state tic date)
                                        ;dummy (pprint update)
]
        (do
          ;(pprint update)
          (j/update-graph!
           (:chart update)
           (:plotly-data update))
          (Thread/sleep 10)))) (if eod?

                                 (do
                                   (reset! (:allowed-to-enter? agent) false) (when (not (zero? current-position))
                                                                               (swap! (:position agent) - current-position)
                                                                               (swap! (:n-trades agent) inc)
                                                                               (reset! (:eod-position agent) current-position) (if (pos? current-position) (do (swap! (:cash-account agent) + (* current-position-abs bidprice))
                                                                                                                                                               (timbre/debug (red "SELL to close " " price " price " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl))
                                                                                                                                                               (if live? (execution/SELL ib-tic current-position-abs)))

                                                                                                                                   (do (swap! (:cash-account agent) - (* current-position-abs askprice))
                                                                                                                                       (timbre/debug (green "BUY to close " " price " price " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl))
                                                                                                                                       (if live? (execution/BUY ib-tic current-position-abs))))

                                                                               (reset! (:total-pnl agent) (int (- @(:cash-account agent) (* commission @(:n-trades agent)))))
                                                                               (reset! (:m2m-pnl agent) @(:cash-account agent))
;; eod pnl can be max pnl or min
                                                                               (let [pnl @(:m2m-pnl agent)]
                                                                                 (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
                                                                                 (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl)))

                                                                               (let [net-pnl   (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))]
                                                                                 (if (pos? net-pnl)
                                                                                   (timbre/debug (green "STATUS " " stock " ib-tic " current-position " @(:position agent) " n-trades " @(:n-trades agent) " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " net-pnl "
                                                                                                        net-pnl))
                                                                                   (timbre/debug (red "STATUS " " stock " ib-tic " current-position " @(:position agent) " n-trades " @(:n-trades agent) " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " net-pnl "
                                                                                                      net-pnl))))

                                                                               (spit log-file-name (str (name date) " stock " ib-tic " agent-name " agent-name " n-trades " @(:n-trades agent)  " max-pos " @(:max-position agent)  " min-pos "  @(:min-position agent) " eod position "  @(:eod-position agent)     " max-pnl " (int @(:max-pnl agent)) " min-pnl " (int @(:min-pnl agent))  " net-pnl " (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))        "\n") :append true))
          ;; at EOD, the current list of prices is the list of positions we have, so pnl is just sum of these pnls
)
      ;;else not eod so do regular stuff

;; write logic here
                                 (do
                                   (cond (and (pos? share-diff) (>= share-diff-abs threshold))

                                         (do
                                           (timbre/debug (green "BUY " " tic " ib-tic " askprice " askprice " share-diff " share-diff " share-diff abs " share-diff-abs " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl))
                                           (swap! (:n-trades agent) inc)
                                           (swap! (:position agent) + share-diff-abs)
                                           (swap! (:cash-account agent) - (* share-diff-abs askprice))
                                           (if live? (execution/BUY ib-tic share-diff-abs))
         ;; buying so decrease wins
)

                                         (and (neg? share-diff) (>= share-diff-abs threshold))
                                         (do
                                           (timbre/debug (red "SELL " " tic " ib-tic " bidprice " bidprice " share-diff " share-diff " share-diff abs " share-diff-abs " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl))

                                           (swap! (:n-trades agent) inc)
                                           (swap! (:position agent)  - share-diff-abs)
                                           (swap! (:cash-account agent) + (* share-diff-abs bidprice))
         ;; buying so decrease wins
                                           (if live? (execution/SELL ib-tic share-diff-abs))))
                                   (reset! (:m2m-pnl agent) (+ cash-account (* current-position reference-price)))
                                   (let [position @(:position agent)]
                                     (if (> position @(:max-position agent)) (reset! (:max-position agent) position))
                                     (if (< position @(:min-position agent)) (reset! (:min-position agent) position)))
                                   (let [pnl @(:m2m-pnl agent)]
                                     (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
                                     (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl))))

;;consider entry
)  ;; eod match
))

;;trading logic
(defn trading-logic-datagen
  "core function, called only when bar data is updated.
to do: otl loss, global max loss ttc cuts larger position
"
  [datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)

        ib-tic (name tic)

        datagen-output (cond (nil? (:datagen agent)) nil
                             :else ((:datagen agent) indicator-state tic date))

        datagen-file (str utils/HOME "/data/1_min/" (str/lower-case ib-tic)  ".us.txt")

        first-datapoint? (not (.exists (io/as-file datagen-file)))

        time @(:seq-no @((indicator-state tic) date))
        ;dummy (log/debug "Trading logic called..")

        ib-tic (name tic)
        ;dummy (log/debug "Trading logic called.." live? ib-tic)
]

    (when datagen?
      (if (every? utils/notnan? (:data datagen-output))
        (timbre/debug (yellow (utils/coll->string (concat (:data datagen-output) ;agent-datagen-output
)))))

      (if first-datapoint?
        (spit datagen-file (utils/coll->string (concat (:header datagen-output) ;agent-datagen-header
                                                       )):append true)
        (if (every? utils/notnan? (:data datagen-output))
          (spit datagen-file (utils/coll->string (concat (:data datagen-output) ;agent-datagen-output
                                                         )):append true))))))

;; new


;;;;;;; this is a different logic, cross sectional entry exit

(defn trading-logic-cross-sectional
  "core function, called only when bar data is updated.
market making
commit is true means you are ready to commit to a band and wait
this is when volatility is low, can be a fn of indicators
calculate band at all times as price+-dp when commit is false
if commit is true, then commit to band, dont update band, watch prices and enter if true
trade n times
for now i only exit on profit goals, ideally this exit is based on other side of band
tic and date are keyword here
"
  [log-file-name datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        price-bars @(@((indicator-state tic) date) :price-bars)
                                        ;sigma (incanter/sd price-bars)

        ;price-band [ (utils/round (- price sigma)) (utils/round (+ price sigma)) ]
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)

        entry-condition (first ((:entry agent) indicator-state tic date))
        exit-condition (cond (nil? (:entry agent)) false :else (nth ((:entry agent) indicator-state tic date) 2)) short-entry-condition (second ((:entry agent) indicator-state tic date))
        short-exit-condition (cond (nil? (:entry agent)) false :else (nth ((:entry agent) indicator-state tic date) 3));; reversed for momentum


                                        ;uts (agent :uts)
        capital 1000.0
        uts (:data (indicators/position-to-take capital indicator-state tic date))
        current-position @(agent :position)
        current-position-abs (incanter/abs current-position)
        exit? (cond (pos? current-position) short-entry-condition (neg? current-position) entry-condition :else false)
        current-n-trades @(agent :n-trades)
        allowed-to-enter? (and @(:allowed-to-enter? agent) (< current-n-trades (agent :max-n-trades)))
        ;;correct pnl accounting, if long i can get out on bid only
        reference-price (cond (pos? current-position) bidprice (neg? current-position) askprice :else price)
        mur (agent :mur)
        ;pnl (agent :pnl)
        last-entry @(agent :last-entry-price)
        agent-name (agent :agent-name)

        last-entry-time @(agent :last-entry-time)
        time-now @(@((indicator-state tic) date) :time)
        ;ttc-now-minutes (cond (zero? current-position) 0.0 :else (utils/round (/ (- time-now last-entry-time) 60000 )) )
        pnls (cond (empty? last-entry) '(0.0) :else (map utils/round (map #(* % current-position) (map - (repeat (count last-entry) reference-price) last-entry))))
        pnls-bps (cond (empty? last-entry) '(0.0) :else
                       (map utils/round (map
                                         #(* % (utils/sign current-position))  (map utils/return-bps (repeat (count last-entry) reference-price)  last-entry))))

        pnl (utils/round (stats/mean pnls))
        pnl-bps (utils/round (stats/mean pnls-bps))
        max-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps
        total-pnl @(:total-pnl agent) ;; this is in dollars now
        m2m-pnl-bps @(:m2m-pnl-bps agent) ;; this is in bps

        drawdown (- max-pnl-this-round pnl-bps)
        ;dummy (pprint current-position)
        pnl-target-reached? (cond (nil? (:pnl-target-bps agent)) false :else (>= pnl-bps (:pnl-target-bps agent)))
        drawdown-breached? (cond (nil? (:max-draw-bps agent)) false :else (>= drawdown (:max-draw-bps agent)))
        ;ttc-breached? (cond (nil? ttc-now-minutes ) false :else (>= ttc-now-minutes (:max-ttc-minutes agent ))  )


        eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.5
        first-datapoint? (not (.exists (io/as-file datagen-file)))
        agent-datagen-output [agent-name current-position pnl m2m-pnl-bps max-pnl-this-round total-pnl drawdown bidprice askprice (first last-entry)]
        agent-datagen-header ["agent-name" "current-position" "pnl" "m2m-pnl-bps" "max-pnl-this-round" "total-pnl" "drawdown" "bidprice" "askprice" "last-entry"]

        datagen-output (cond (nil? (:datagen agent)) nil
                             :else ((:datagen agent) indicator-state tic date))
        live? (cond (nil? @(:live? agent)) false :else @(:live? agent)) ;; place orders
        time @(:seq-no @((indicator-state tic) date))
        ib-tic (name tic)
        cash-account @(:cash-account agent) ;; this is in bps
        jutsu? (cond (nil? (:jutsu agent)) false :else (:jutsu agent))
        m2m-pnl @(:m2m-pnl agent)]
    ;;remember eod positions and total-pnls are not correct in datagen bcos
    ;; after eod it does not print another point
    (if (and datagen? (:print-datagen? agent)) (if first-datapoint?
                                                 (spit datagen-file (utils/coll->string (concat (:header datagen-output) agent-datagen-header)) :append true)
                                                 (spit datagen-file (utils/coll->string (concat (:data datagen-output) agent-datagen-output)) :append true)))
    (if (and eod? (not eod-plus-one?))
      (do

        (when (not (zero? current-position))
          (swap! (:position agent) - current-position)
          (reset! (:eod-position agent) current-position)
          (swap! (:n-trades agent) inc)

          (if (pos? current-position) (swap! (:cash-account agent) + (* current-position-abs bidprice)) (swap! (:cash-account agent) - (* current-position-abs askprice)))
          (reset! (:allowed-to-enter? agent) false)
          (swap! (:total-pnl agent) + (int (- @(:cash-account agent) (* commission @(:n-trades agent)))))
          (if (pos? current-position)
            (do (execution/SELL ib-tic (incanter/abs current-position))
                (timbre/debug (red "EODSELLCLOSE " " tic " ib-tic " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl   " total-pnl " (:total-pnl agent))))
            (do (execution/BUY ib-tic (incanter/abs current-position))
                (timbre/debug (green "EODBUYCLOSE " " tic " ib-tic " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl   " total-pnl " (:total-pnl agent)))))
          (reset! (:m2m-pnl agent) @(:cash-account agent))
;; eod pnl can be max pnl or min
          (let [pnl @(:m2m-pnl agent)]
            (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
            (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl))))
        (spit log-file-name (str (name date) " stock " ib-tic " agent-name " agent-name " n-trades " @(:n-trades agent)  " max-pos " @(:max-position agent)  " min-pos "  @(:min-position agent) " eod position "  @(:eod-position agent)     " max-pnl " (int @(:max-pnl agent)) " min-pnl " (int @(:min-pnl agent))  " net-pnl " (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))        "\n") :append true)

        (let [net-pnl   (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent))))])

     ;; at EOD, the current list of prices is the list of positions we have, so pnl is just sum of these pnls
)
      (do
        (cond

          (zero? current-position)
          (do (when (and entry-condition allowed-to-enter?)

                (do

                  (swap! (:position agent) + uts)
                  (swap! (:last-entry-price agent) conj askprice)
                  (reset! (:last-entry-time agent) time-now)

                  (swap! (:cash-account agent) - (* uts askprice))

                  (execution/buy-hedge capital indicator-state tic date)
                  (timbre/debug (green "BUY " " tic " ib-tic  " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl " total-pnl " total-pnl)) (swap! (:n-trades agent) inc))
                                        ;  (enterlong)
)
              (when (and short-entry-condition allowed-to-enter?)
                                        ;(pprint "short inc enter...")
                (do
                  (swap! (:position agent) - uts)
                  (swap! (:last-entry-price agent) conj bidprice)
                  (reset! (:last-entry-time agent) time-now)
                                        ;(timbre/debug "check band... time tic price-band1 2" time tic @(:price-band-lower agent ) @(:price-band-upper agent )  )
                                        ;(timbre/debug "Enter short... time tic price pnl" time tic price @(:total-pnl agent))
                  (swap! (:cash-account agent) + (* uts bidprice))
                                        ;(timbre/debug "sell-hedge call" capital indicator-state tic date )
                  (execution/sell-hedge capital indicator-state tic date)
                  (timbre/debug (yellow "SELL " " tic " ib-tic " price " bidprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl " total-pnl " total-pnl)) (swap! (:n-trades agent) inc))))
          (not (zero? current-position))  ;; update pnls
          (do (reset! (:m2m-pnl-bps agent) pnl-bps)
              (when (> pnl-bps max-pnl-this-round)
                (reset! (:max-pnl-this-round agent) pnl-bps))
              (when (or pnl-target-reached? drawdown-breached? exit-condition short-exit-condition) ;; change exit idea
        ;;time to get out
                                        ;(pprint (str "pnl target reached, consider exit at pnl " (first pnls)))
                                        ;  (exitposition)
                (cond
                  (> current-position 0)
                  (do
                    (swap! (:position agent) - (incanter/abs current-position))
                    (swap! (:last-exit-price agent) conj price)
            ;(swap! (:last-entry-price agent) pop )
            ;(swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)) )
                    (reset! (:m2m-pnl-bps agent) 0.0)
                    (reset! (:max-pnl-this-round agent) 0.0)
                    (reset! (:last-entry-time agent) nil)
                    (swap! (:n-trades agent) inc)

                    (swap! (:cash-account agent) + (* current-position-abs bidprice))
                    (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent)))))
            ;(execution/sell-hedge capital indicator-state tic date )
                    (timbre/debug (yellow "SELLCLOSE " " tic " ib-tic " price " bidprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl  " total-pnl " total-pnl));(timbre/debug "Exit long... time tic price pnl" time tic price @(:total-pnl agent))
)
                  (< current-position 0)
                  (do
                    (swap! (:position agent) + (incanter/abs current-position))
                    (swap! (:last-exit-price agent) conj price)
            ;(swap! (:last-entry-price agent) pop )
                    (swap! (:total-pnl agent)   + (- (first pnls) (* 2 commission)))
                    (reset! (:m2m-pnl-bps agent) 0.0)
                    (reset! (:max-pnl-this-round agent) 0.0)
                    (reset! (:last-entry-time agent) nil)
                    (swap! (:n-trades agent) inc)

                    (swap! (:cash-account agent) - (* current-position-abs askprice))
                    (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent)))))
            ;(execution/buy-hedge capital indicator-state tic date )
                    (timbre/debug (green "BUYCLOSE " " tic " ib-tic " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl   " total-pnl " total-pnl));(timbre/debug "Exit short... time tic price pnl" time tic price @(:total-pnl agent))
)))))

        (reset! (:m2m-pnl agent) (+ @(:cash-account agent) (* @(:position agent) reference-price)))
        (let [position @(:position agent)]
          (if (> position @(:max-position agent)) (reset! (:max-position agent) position))
          (if (< position @(:min-position agent)) (reset! (:min-position agent) position)))
        (let [pnl @(:m2m-pnl agent)]
          (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
          (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl))) (do (comment (timbre/debug (green "STATUS " " stock " ib-tic " current-position " @(:position agent) " n-trades " @(:n-trades agent) " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " net-pnl " (utils/round (- @(:cash-account agent) (* commission @(:n-trades agent)))))))
      ;((indicator-state tic) date)
                                                                            (reset! (@((indicator-state tic) date) :pnl)   @(:m2m-pnl agent))));; if commit true, then lock commit-lock into true, stop updating price band
)))

;; trading logic, called for each strat, tic date


;; add watch to both prices and eod flag
;; now define this with indicator state as input
(defn add-callback-agent
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic datagen? agent indicator-state tic date)))))

(defn add-callback-agent-make-markets
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [log-file-name datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-make-markets log-file-name datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-make-markets log-file-name datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-make-markets log-file-name datagen? agent indicator-state tic date)))))

(defn add-callback-agent-cross-sectional
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [log-file-name datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-cross-sectional log-file-name datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-cross-sectional log-file-name datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-cross-sectional log-file-name datagen? agent indicator-state tic date)))))

(defn add-callback-agent-position-target
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [log-file-name datagen-file datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-position-target log-file-name datagen-file datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-position-target log-file-name datagen-file datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-position-target log-file-name datagen-file datagen? agent indicator-state tic date)))))

(defn add-callback-agent-position-target-limit
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [log-file-name datagen-file datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-position-target-limit log-file-name datagen-file datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-position-target-limit log-file-name datagen-file datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-position-target-limit log-file-name datagen-file datagen? agent indicator-state tic date)))))

(defn add-callback-agent-debug
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [log-file-name datagen-file datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-debug log-file-name datagen-file datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-debug log-file-name datagen-file datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-debug log-file-name datagen-file datagen? agent indicator-state tic date)))))

(defn add-callback-agent-follow-trend
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-follow-trend datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-follow-trend datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-follow-trend datagen? agent indicator-state tic date)))))

(defn add-callback-agent-fernholz
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [log-file-name datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-fernholz log-file-name datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-fernholz log-file-name datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (trading-logic-fernholz log-file-name datagen? agent indicator-state tic date)))))

(defn add-callback-agent-datagen
  "for indicator-state object and an agent, subscribe the agent to all the indicators states of tics and dates - agent is listening to all M*N price-bars"
  [datagen? indicator-state agent tic date]
  (do (add-watch (:price-bars @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-datagen datagen? agent indicator-state tic date)))
      (add-watch (:eod? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-datagen datagen? agent indicator-state tic date)))

;; seems stupid to write datagen one last time after eod call, i do this
      (add-watch (:eod-plus-one? @((indicator-state tic) date)) (keyword (str (gensym (utils/random-word))))
                 (fn [key atom old-state new-state]
                   (tradinglogic/trading-logic-datagen datagen? agent indicator-state tic date)))))

;; sim-strategy takes agent ticker-coll and date-coll


;;helper functions for retrieval

(defn retrieve-pnls [agent-state tic date]
  (let [k-tic (keyword tic)
        k-date (keyword (str date))]
    @(:total-pnl @((agent-state k-tic) k-date))))

(defn retrieve-pnl-sharpe
  "pnl sharpe noofdatapoints min"
  [agent-state tic date]
  (let [k-tic (keyword tic)
        k-date (keyword (str date))
        pnls @(:total-pnl @((agent-state k-tic) k-date))]
    pnls))

(defn retrieve-pnls-tmp [agent-state dates tic] (map (partial retrieve-pnls agent-state tic) dates))

(defn retrieve-pnl-sharpe-tmp [agent-state dates tic] (utils/pnl-sharpe (map (partial retrieve-pnl-sharpe agent-state tic) dates)))

(declare agent-states)
(defn pnls-for-the-day [date-coll ticker] (let [this-ticker ticker
                                                result (flatten (map #(retrieve-pnls-tmp % date-coll this-ticker) agent-states))
                                        ;f- (str "/home/puru/Dropbox/data/ticker-" this-ticker ".data")
]
                            ;(pprint this-ticker)
                                            result
                                        ;(spit f- (pr-str result   ))
))

;; readies indicator matrix
;; readies agent matrix
;; adds callbacks to trading logics
;; runs through tickers and dates
;; reports final pnl map

(defn sim-strategy
  "
 readies indicator matrix
 readies agent matrix
 adds callbacks to trading logics
 runs through tickers and dates
 reports final pnl map

"
  [datagen? agent ticker-coll date-coll]
  (let [indicator-state (create-indicator-state ticker-coll date-coll)
        agent-state (create-agent-state agent ticker-coll date-coll)]
    (doall (for [this-ticker ticker-coll this-date date-coll]
             (let [tic (keyword  this-ticker)
                   date (keyword (str this-date))
                   agent-listening-to-this-node  @((agent-state tic) date)]
               (add-callback-agent datagen? indicator-state agent-listening-to-this-node tic date))))
;;run through tickers
    (doall (for [this-ticker ticker-coll this-date date-coll] (update-data-quotes indicator-state this-ticker this-date)))
;; collect results
    (zipmap (map keyword ticker-coll) (map (partial retrieve-pnls-tmp agent-state date-coll)  ticker-coll))))

(defn sim-strategy-make-markets
  "
 readies indicator matrix
 readies agent matrix
 adds callbacks to trading logics
 runs through tickers and dates
 reports final pnl map

"
  [datagen? agent ticker-coll date-coll]
  (let [indicator-state (create-indicator-state ticker-coll date-coll)
        agent-state (create-agent-state agent ticker-coll date-coll)]
    (doall (for [this-ticker ticker-coll this-date date-coll]
             (let [tic (keyword  this-ticker)
                   date (keyword (str this-date))
                   agent-listening-to-this-node  @((agent-state tic) date)]
               (add-callback-agent-make-markets datagen? indicator-state agent-listening-to-this-node tic date))))
;;run through tickers
    (doall (for [this-ticker ticker-coll this-date date-coll] (update-data-quotes indicator-state this-ticker this-date)))
;; collect results
    (zipmap (map keyword ticker-coll) (map (partial retrieve-pnls-tmp agent-state date-coll)  ticker-coll))))

(defn view-results [f-]
  (let [r (read-string (slurp f-))]
    (reverse (sort-by #(-> % second first) r))))

(defn init-jutsu [ticker]
  {:x [-1]
   :y [0]
   :mode "markers"
   :type "scatter"
   :name ticker})

;; looks like it works
;(sim-strategy (agent-2 60 10 5) ["GGAL" "CSOD"] [20171117 20171120 ])

;; test it
;(def dates [20171113 20171114 20171115 20171116 20171117 ])


;; right now this is ineffecient, cos it queries data repeatedly


;; and this is how you can write to datagen


;(def f- (str "/home/puru/Dropbox/data/results-" (str (gensym)) ".data" ))
;(spit f- (pr-str results-state))


(comment
;defn -main [& args]
;(serv/start-server :port 7888)
;;setup dates
                                        ;defn -main [& args]

                                        ;(pprint (:tickers (parse-opts args cli-options)))

  (def cmd-args (let [{:keys [options arguments summary errors]} (parse-opts args
                                                                             [["-t" "--tickers" "ticker list"
                                                                               :default ["AAPL"]]
                                                                              ["-d" "--start-date" "date"
                                                                               :default [20180108]]
                                                                              ["-d2" "--end-date" "date"
                                                                               :default [20180108]] ["-f" "--file" "file to write results to"
                                                                                                     :default (u/random-word)]
                                                                              ["-h" "--help" "Print this help" :default false]])]
                  arguments));(def cmd-args [20171205 "CGNX,GWPH,PBYI,TTWO,SHOP" "fatwa"])
  (def ticker-coll (str/split (first cmd-args) #","))
;(def ticker-coll (concat ["SPY"] ticker-coll))
  (def date-coll (let [start-date (read-string (second cmd-args))
                       end-date (read-string (nth cmd-args 2))
                       date-coll (filter #(and (>= % start-date) (<= % end-date))
                                         (tickers/recent-dates))]
                   date-coll))
; (def date-coll [ (second cmd-args) ]  )

  (def arg-token (nth cmd-args 3))
  (def token arg-token);(def ticker-coll ["BPMC"])
                                        ;(def date-coll [20171215 20171214])

                                        ;(def arg-token "test")
                                        ;(def token arg-token)

  (comment (j/start-jutsu!)
           (Thread/sleep 3000)

           (doseq [portfolio tickers/portfolio-name-tickers]
             (do
                                        ;            (Thread/sleep 3000)
                                        ;            (j/start-jutsu!)
                                        ;           (Thread/sleep 3000)
               (j/graph!
                (str (first portfolio) "-prices")
                (map init-jutsu (vec (second portfolio))))
               (j/graph!
                (str (first portfolio) "-volumes")
                (map init-jutsu (vec (second portfolio))))
               (comment (j/graph!
                         (str (first portfolio) " LM")
                         (map init-jutsu (vec (second portfolio))))) (let [ticker-coll (second portfolio)
                                                                           p-key (keyword (first portfolio))] (doseq [counter (range (count ticker-coll))]
                                                                                                                (swap! (p-key jutsudata/trace-id-map) assoc-in [(keyword (nth ticker-coll counter))] counter))))))

;(def ticker-coll (sort (flatten (map second tickers/portfolio-name-tickers))))
;; now datagen comes from agent itself
  (def datagen? true)
;(def ticker-coll (concat ticker-coll '("SPY") ))
;(def ticker-coll portfolio)
  ;;cannot be more than 20
                                        ; (def N 20)


                                        ;(def ticker-coll (take N (sort (read-string (slurp "/home/puru/Dropbox/data/mid-tickers.data" )))))
                                        ;(def dates [20171113 20171114 20171115 20171116 20171117 ])
                                        ;(def date-coll (take N (read-string (slurp "/home/puru/Dropbox/data/recent-dates.data"))))
;; setup logging
  (def log-file-name (str utils/HOME "/Dropbox/data/" arg-token ".data"))
  (io/delete-file log-file-name :quiet)

  (def datagen-file (str utils/HOME "/Dropbox/data/" arg-token ".datagen"))
  (io/delete-file datagen-file :quiet)

  (def results-file-name (str utils/HOME "/Dropbox/data/" arg-token ".results")) (do
    ;(def a-list (identity agents/a-list-datagen ))
                                                                                   (def a-list (identity agents/a-list))

                                        ;(if (= "true" datagen?) (do (pprint "Got datagen true, doing only one datagen strat eval..") (def a-list agents/a-list-datagen)))
                                        ;(def source- (rand-nth (drop-last 2 ticker-date-list) ))

                                                                                   (def agent-states (map #(create-agent-state % ticker-coll date-coll) a-list))
                                                                                   (def i-state (create-indicator-state ticker-coll date-coll)))
                                        ;(pprint "here")
  (doseq [agent-state agent-states]
    (doseq [this-ticker ticker-coll]
      (doseq [this-date date-coll]

        (let [tic (keyword  this-ticker)
              date (keyword (str this-date))
              agent-listening-to-this-node  @((agent-state tic) date)]
                                        ;(pprint (str tic " " date))
;                                                (add-callback-agent-make-markets log-file-name datagen? i-state agent-listening-to-this-node tic date )
;          (add-callback-agent-follow-trend datagen? i-state agent-listening-to-this-node tic date )
;          (add-callback-agent-fernholz log-file-name datagen? i-state agent-listening-to-this-node tic date )
          ;(add-callback-agent-datagen datagen? i-state agent-listening-to-this-node tic date )
          ;(add-callback-agent-cross-sectional log-file-name datagen? i-state agent-listening-to-this-node tic date )
          ;(add-callback-agent-position-target log-file-name datagen-file datagen? i-state agent-listening-to-this-node tic date )
          (add-callback-agent-position-target-limit log-file-name datagen-file datagen? i-state agent-listening-to-this-node tic date)))))

  (pprint "Doing...");; from here backtest system will diverge from live system
 ;; this is backtest

 ;; setup logging

; The default setup is simple console logging.  We with to turn off console logging and
; turn on file logging to our chosen filename.
;(timbre/set-config! [:appenders :standard-out   :enabled?] false)
;(timbre/set-config! [:appenders :spit           :enabled?] true)
;(timbre/set-config! [:shared-appender-config :spit-filename] log-file-name)
;(timbre/set-config! [:shared-appender-config :spit-filename] log-file-name)
 ;; end setup logging


  (doseq
   [this-date date-coll]
    ;(pprint (str "Doing this tic and date " this-ticker " " this-date))
    (update-data-quotes-new i-state ticker-coll  this-date)) (def total-pnl (reduce + (map #(:data (indicators/pnl-indicator i-state (keyword %) (keyword (str (first date-coll))))) ticker-coll)))

  (def total-pnls (map #(:data (indicators/pnl-indicator i-state (keyword %) (keyword (str (first date-coll))))) ticker-coll)) (pprint (str "Total pnl -> " ticker-coll " " (first date-coll) " " total-pnl))
  (pprint "pnls by ticker -> ")
  (pprint (map vector ticker-coll total-pnls))
  (def write-this (str (first date-coll) " " (str/join "," ticker-coll)  " " total-pnl "\n"))
  (spit results-file-name write-this :append true)
  (shutdown-agents)) ;;end main


;; live trading main
(;comment
  defn -main [& args]
  (serv/start-server :port 7888)

  (def cmd-args (let [{:keys [options arguments summary errors]} (parse-opts args
                                                                             [["-t" "--tickers" "ticker list"
                                                                               :default ["AAPL"]]])]

                  arguments));(def cmd-args [20171205 "CGNX,GWPH,PBYI,TTWO,SHOP" "fatwa"])
  (def ticker-coll (str/split (first cmd-args) #","))
;(def ticker-coll (str/split (slurp "/home/puru/Dropbox/data/corr_tickers.data") #"\n"))
  (def ticker-coll (take 5 (shuffle (filter tickers/good-ticker? tickers/tickers))))
  (def ticker-coll (concat ["SPY"] ticker-coll))
  (comment (j/start-jutsu!)
           (Thread/sleep 3000)
           (doseq [portfolio tickers/portfolio-name-tickers]
             (do
            ;(j/start-jutsu!)
               (Thread/sleep 3000)
               (j/graph!
                (str (first portfolio) "-prices")

                (let [out (atom [])]
                  (doseq [ticker (vec (second portfolio))]
                    (swap! out conj (init-jutsu ticker)))
                  @out))
               (Thread/sleep 3000)
               (j/graph!
                (str (first portfolio) "-volumes")
             ;(map init-jutsu (vec (second portfolio)))

                (let [out (atom [])]
                  (doseq [ticker (vec (second portfolio))]
                    (swap! out conj (init-jutsu ticker)))
                  @out))
               (Thread/sleep 3000)

               (comment
                 (str (first portfolio) "-realprices")
                 (map init-jutsu (vec (second portfolio)))) (let [ticker-coll (second portfolio)
                                                                  p-key (keyword (first portfolio))] (doseq [counter (range (count ticker-coll))]
                                                                                                       (swap! (p-key jutsudata/trace-id-map) assoc-in [(keyword (nth ticker-coll counter))] counter))))))

  (defn forever [indicator-state ticker-coll date]
    (do
      (doseq [ticker ticker-coll]
        (j/update-graph!
         "prices"
         (:jutsu (indicators/returns-from-open indicator-state (keyword ticker) (keyword (str date)))))
        (Thread/sleep 10)

        (timbre/debug "Sent update to graph "  (:jutsu (indicators/returns-from-open indicator-state (keyword ticker) (keyword (str date))))))
      (Thread/sleep bardata-frequency-msecs)))

  (defn forever-pnl [indicator-state ticker-coll date]
    (do
      (doseq [ticker ticker-coll])
  ;(Thread/sleep bardata-frequency-msecs)
))

;(def ticker-coll (sort (flatten (map second tickers/portfolio-tickers-relevant))))
;(def ticker-coll portfolio)


  (def datagen? false)
  (def log-file-name (str utils/HOME "/Dropbox/data/" "live" ".data"))
  (io/delete-file log-file-name :quiet)

  (def datagen-file (str utils/HOME "/Dropbox/data/" "live" ".datagen"))
  (io/delete-file datagen-file :quiet)

;  (def ticker-coll ["CRSP"])

  (def date-coll [@tickers/today_int])

  (do
    (def a-list (identity agents/a-list))
    (def agent-states (map #(create-agent-state % ticker-coll date-coll) a-list))
    (def i-state (create-indicator-state ticker-coll date-coll)))
                                        ;(pprint "here")
  (doseq [agent-state agent-states]
    (doseq [this-ticker ticker-coll]
      (doseq [this-date date-coll]

        (let [tic (keyword  this-ticker)
              date (keyword (str this-date))
              agent-listening-to-this-node  @((agent-state tic) date)]
          (pprint (str tic " adding callbacks " date))
         ;(add-callback-agent-make-markets log-file-name datagen? i-state agent-listening-to-this-node tic date )
         ;(add-callback-agent-cross-sectional log-file-name datagen? i-state agent-listening-to-this-node tic date )
                                        ;(add-callback-agent-position-target log-file-name datagen-file datagen? i-state agent-listening-to-this-node tic date )
          (add-callback-agent-position-target-limit log-file-name datagen-file datagen? i-state agent-listening-to-this-node tic date)

;        (add-callback-agent-follow-trend datagen? i-state agent-listening-to-this-node tic date )
        ;(add-callback-agent-fernholz log-file-name datagen? i-state agent-listening-to-this-node tic date )
))))

  (def streamURL (str "http://localhost:5000/quoteStream?symbol=" (clojure.string/join "+" ticker-coll)))
  (println streamURL)

  (defn trades-chan
    "Open the URL as a stream of trades information. Return a channel of the trades, represented as strings."
    [url]
    (let [lines (-> url
                    (client/get {:as :stream})
                    :body
                    io/reader
                    line-seq)];;A lazy seq of each line in the stream.
      (clojurecoreasync/to-chan lines))) ;;Return a channel which outputs the lines
  (def live-date (first date-coll))

  ;; view jutsu data forever
  (comment
    (Thread. (forever i-state ticker-coll live-date))
    (.setDaemon true)
    (.start)) (defn onTick
                [msg]
 ;(pprint msg)
                (if (= \T (first msg)) (parse-stream-trade i-state live-date msg)
                    (if (= \Q (first msg)) (parse-stream-quote i-state live-date msg))))

;;;;;;;position sizing logic


  (println "Trying Subscription to data...")
  (def msgBlock (trades-chan streamURL))
  (println "Subscribed to data...")

  (println "Trying Connection to gateway...")
  (gateway/connect)
  (gateway/subscribe prn)
  (println "Done...")

  (Thread/sleep 1000)
  (println "account updates...")
 ;(gateway/request-account-updates true "1")
  (println "Done...")
  (Thread/sleep 1000) (clojurecoreasync/go-loop [takes 100]
                        (when (< 0 takes)
                          (onTick (clojurecoreasync/<! msgBlock))
                          (recur takes)))

;; doesnt work
  (comment (dotimes [_ 390]
             (doseq [ticker ticker-coll]
               (j/update-graph!
                (str (tickers/ticker->sector ticker) "-prices")
                (:jutsu (indicators/returns-from-open i-state (keyword ticker) (keyword (str live-date)))))
               (Thread/sleep 10)
               (j/update-graph!
                (str (tickers/ticker->sector ticker) "-volumes")
                (:jutsu (indicators/cumulative-relative-volume i-state (keyword ticker) (keyword (str live-date)))))
               (Thread/sleep 10)

               (comment
                 (str (tickers/ticker->sector ticker) "-realprices")
                 (:jutsu (indicators/l1-price i-state (keyword ticker) (keyword (str live-date)))))
                                        ;(Thread/sleep 10)


               (timbre/debug "Sent update to graph "  (:jutsu (indicators/returns-from-open i-state (keyword ticker) (keyword (str live-date))))))
             (Thread/sleep bardata-frequency-msecs))) (Thread/sleep (* 380 60 1000))

  (doseq [ticker ticker-coll]
    (doseq [date date-coll]
      (reset! (@((i-state (keyword ticker)) (keyword (str date))) :eod?)  true)
 ;(reset! (@((indicator-state (keyword ticker)) (keyword (str date))) :eod-plus-one?)  true)
))

  (pprint "Getting EOD Flat...")
  (pprint "Getting EOD Flat...")
  (pprint "Getting EOD Flat...")
  (pprint "Getting EOD Flat...")
  (pprint "Getting EOD Flat...")
  (pprint "Getting EOD Flat...")

  (Thread/sleep (* 3 60 1000))  ;; 3 minutes to get flat


;;raise eod flags to be true

  (shutdown-agents))   ;;end live main
;(map class (tickers/recent-dates))

;(def ticker-coll ["AAPL" "BARD"])


       ;(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name :HCP)))) :HCP )


(comment ;; to send user_msg


  (do

    (def ticker-coll (str/split "SPLK" #","))
  ;(def ticker-coll ["AMBA" "PSTG" "SPY" ])
    (def portfolio ticker-coll)
    (def date-coll [20180302])
                                        ;(def a-list (identity agents/a-list-datagen ))
    (def a-list (identity agents/a-list))
    (def agent-states (map #(create-agent-state % ticker-coll date-coll) a-list))

    (def a-list-datagen (identity agents/a-list-datagen))
    (def agent-states-datagen (map #(create-agent-state % ticker-coll date-coll) a-list)) (def i-state (create-indicator-state ticker-coll date-coll))

    (def log-file-name (str utils/HOME "/Dropbox/data/" "test" ".data"))
    (io/delete-file log-file-name :quiet)
    (def datagen-file (str utils/HOME "/Dropbox/data/" "test" ".datagen"))
    (io/delete-file datagen-file :quiet)

    (def datagen? false)
    (pprint portfolio))
                                        ;(pprint "here")
  (doseq [agent-state agent-states]
    (doseq [this-ticker ticker-coll]
      (doseq [this-date date-coll]

        (let [tic (keyword  this-ticker)
              date (keyword (str this-date))
              agent-listening-to-this-node  @((agent-state tic) date)]
          (pprint (str tic " " date))
                                                ;(add-callback-agent-make-markets log-file-name datagen? i-state agent-listening-to-this-node tic date )
;          (add-callback-agent-follow-trend datagen? i-state agent-listening-to-this-node tic date )
;          (add-callback-agent-fernholz log-file-name datagen? i-state agent-listening-to-this-node tic date )
          ;(add-callback-agent-datagen datagen? i-state agent-listening-to-this-node tic date )
                ;                        (add-callback-agent-cross-sectional log-file-name datagen? i-state agent-listening-to-this-node tic date )
          (add-callback-agent-position-target log-file-name datagen-file datagen? i-state agent-listening-to-this-node tic date))))) (def datagen? true)

  (doseq [agent-state agent-states-datagen]
    (doseq [this-ticker ticker-coll]
      (doseq [this-date date-coll]

        (let [tic (keyword  this-ticker)
              date (keyword (str this-date))
              agent-listening-to-this-node  @((agent-state tic) date)]
          (pprint (str tic "-datagen- " date))
                                                ;(add-callback-agent-make-markets log-file-name datagen? i-state agent-listening-to-this-node tic date )
;          (add-callback-agent-follow-trend datagen? i-state agent-listening-to-this-node tic date )
;          (add-callback-agent-fernholz log-file-name datagen? i-state agent-listening-to-this-node tic date )
          (add-callback-agent-datagen datagen? i-state agent-listening-to-this-node tic date)
                ;                        (add-callback-agent-cross-sectional log-file-name datagen? i-state agent-listening-to-this-node tic date )
          )))))(comment
                 (doseq
                  [this-date date-coll]
                   (update-data-quotes-new i-state portfolio this-date)))

;(identity @tickers/today_int)


;(getTradesQuotesNew "AAPL" 20180125 "093000" "103000")

;(pprint (count (getTradesQuotesNew-extended ["SPY" "AAPL"] 20180125 )))


;(def msgs (getTradesQuotes-extended-new (take 2 portfolio) 20171114))
;(pprint (take 2 msgs))
;(parse-stream-trade-backtest-new i-state (keyword (str 20171114)) (first msgs))
;(getTradesQuotes-extended-new-premarket ["AAPL"] 20180202 )

;; work for pre market stuff


;(spit "/home/puru/data/bkd.tics"(str/join "\n"(take 2000 (str/split (getTradesQuotes-extended "BKD" 20180222 ) #"\n"))))


;(["CPRT" -587.4])
;(["GKOS" -665.35] ["M" -338.16] ["DDS" 596.87])
;IndexOutOfBoundsException   clojure.lang.PersistentVector.arrayFor (PersistentVector.java:158)
;()
;(["SPLK" 5.46])
;ArithmeticException Divide by zero  clojure.lang.Numbers.divide (Numbers.java:158)
;(["DDD" 84.48] ["MU" 14.97] ["EVH" 163.93] ["XL" -222.62])
;(["CREE" -152.29] ["BZUN" 1248.67])
