(ns matrix.tradinglogic

  (:require
   [matrix.utils :as utils]

   [matrix.trackorders :as trackorders]
   [matrix.timekeeper :as timekeeper]

   [matrix.indicators :as indicators]
   [matrix.agents :as agents]
   [matrix.tickers :as tickers]
   [matrix.jutsudata :as jutsudata]

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
   [clojure.term.colors :refer :all]

; white, cyan, magenta, blue, yellow, green, red, grey, on-white,
;on-cyan, on-magenta, on-blue, on-yellow, on-green, on-red, on-grey,
;concealed, reverse-color, blink, underline, dark, bold
   )(:use clojure.pprint)
  (:require [clojure.tools.cli :refer [parse-opts]])

  (:gen-class))

;;;;;;; this is a different logic, cross sectional entry exit
(declare datagen-file)
;         :max-open-trade-loss 50.0
;         :break-after-otl-minutes 5.0

;; add drawdown cooloff for five minutes, clearly your signal is false now, prices are falling(rising) and signal says buy(sell) ; allow opposite entry add allowed-to-enter-long and short
;; write recent peak indicator, shows direction of displacement from most recent peak or trough to replace returns from open -> lets call it neeyat (teri neeyat kya hai?)
;; maybe not take anti sentiment positions, ie ones that are opposite open(today)-yesterday(close) -> on second thought no, neeyat indicator will decide where it is going
;; keep track of max(total-pnl) - total-pnl -> when maxdd is hit, stop trading. you're done.
;; enter only when dispersion is moving? prevent nokia type of trading


(defn trading-logic-position-target
  "
core function, called only when bar data is updated.
strong signal -> target position is positive
weak signal -> exit
strong short signal -> target position is negative
weak signal -> exit

"
  [log-file-name datagen-file datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        price-bars @(@((indicator-state tic) date) :price-bars)
                                        ;sigma (incanter/sd price-bars)

        ;price-band [ (utils/round (- price sigma)) (utils/round (+ price sigma)) ]
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)
        spread (utils/round (- askprice bidprice))

        target-position-prior ((:target-position agent) indicator-state tic date)
        exit-position ((:exit-position agent) indicator-state tic date)
        ;reference-price ( (:reference-price agent) indicator-state tic date) ;; only units 0 -> 5 -> -5

        capital 10000.0
        max-uts (:data (indicators/position-to-take capital indicator-state tic date)) ;; the jump from 0 is this position

        calc-uts ((:target-size agent) indicator-state tic date) ;; can be NaN
        uts (apply min [max-uts calc-uts])
        ;market-uts (:data (indicators/position-to-take capital indicator-state :SPY date)) ;; the jump from 0 is this position
        current-position @(agent :position)
        current-position-abs (incanter/abs current-position)
        current-position-status @(agent :position-status)  ;; this is + 1 -1 or 0 ; long short or flat
        getflat? (and (not (zero? exit-position)) (not (zero? current-position-status)) (not= current-position-status exit-position))
        ;target-position (cond (neg? (* current-position target-position-prior)) 0.0 :else target-position-prior )
        ;; if we have position ignore signal and use drawdowns to exit (or ttc) - a hack
        target-position (cond (not (zero? current-position)) current-position-status :else target-position-prior)  ;;t-p is always a unit quantity

        position-diff (cond (Double/isNaN target-position) 0.0 :else (- target-position current-position-status))
        position-diff-abs (incanter/abs position-diff) unsigned-actual-trade-size (* uts position-diff-abs)  ;; typically 1 or 2 times uts, signed quantity ; causes residual positions for now fix later
        ;unsigned-market-trade-size (* market-uts position-diff-abs)
        current-n-trades @(agent :n-trades)
        current-n-otls @(agent :n-otls)

;;correct pnl accounting, if long i can get out on bid only
        ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )

        agent-name (agent :agent-name) time-now @(@((indicator-state tic) date) :time)
        time-string @(@((indicator-state tic) date) :time-string)
        ;max-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps
        total-pnl @(:total-pnl agent) ;; this is in dollars now

        eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.5
        first-datapoint? (not (.exists (io/as-file datagen-file))) datagen-output (cond (nil? (:datagen agent)) nil
                                                                                        :else ((:datagen agent) indicator-state tic date))
        live? (cond (nil? @(:live? agent)) false :else @(:live? agent)) ;; place orders
        time @(:seq-no @((indicator-state tic) date))
        ib-tic (name tic)
        cash-account @(:cash-account agent) ;; this is in bps

        m2m-pnl @(:m2m-pnl agent)
        max-pnl @(:max-pnl agent)
        draw (- max-pnl m2m-pnl)
        allowed-drawdown (utils/drawdown-at-pnl (apply max [max-pnl total-pnl]))
        drawdown-breached? (> draw allowed-drawdown)
        tentative-max-otl 100.0
        ;(apply max [ ( (:max-otl2 agent) indicator-state tic date) 100.0  ])
        allowed-to-enter-long? (and (not= tic :SPY) (not (Double/isNaN uts)) (not (Double/isNaN tentative-max-otl)) @(:allowed-to-enter-long? agent) (< current-n-trades (agent :max-n-trades)) (< @(agent :n-otls) (agent :max-otls))   (<= spread (agent :max-spread)))

        allowed-to-enter-short? (and (not= tic :SPY) (not (Double/isNaN uts)) (not (Double/isNaN tentative-max-otl)) @(:allowed-to-enter-short? agent) (< current-n-trades (agent :max-n-trades))  (< @(agent :n-otls) (agent :max-otls))   (<= spread (agent :max-spread)))
;;correct pnl accounting, if long i can get out on bid only
        ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )
        reference-price ((:reference-price agent) indicator-state tic date) otl-breached? (>= (- m2m-pnl) @(:max-otl agent))
        otl-sleep (* 60000 (:break-after-otl-minutes agent))
        time-since-last-otl-msecs (- time-now @(:otl-start-time agent));; now i take care of max-total-pnl, where a good thing is preserved
        max-total-pnl @(:max-total-pnl agent)

        ;m2m-pnl-relevant (cond (neg? m2m-pnl) (- m2m-pnl) :else 0) ;; only if negative i worry
        go-home-pnl (+ total-pnl m2m-pnl) ;; this is my total-pnl-draw
        total-pnl-draw (- max-total-pnl go-home-pnl)
        total-allowed-drawdown (utils/drawdown-at-pnl max-total-pnl)
        total-drawdown-breached? (> total-pnl-draw total-allowed-drawdown)
        last-entry-time @(:last-entry-time agent)
        ttc-now-minutes (cond (zero? current-position) 0.0 :else (utils/round (/ (- time-now last-entry-time) 60000)))
        ttc-breached? (cond (nil? ttc-now-minutes) false :else (>= ttc-now-minutes (:max-ttc-minutes agent)))]
    ;;remember eod positions and total-pnls are not correct in datagen bcos
    ;; after eod it does not print another point
    (if (and datagen? (:print-datagen? agent)) (if first-datapoint?
                                                 (spit datagen-file (utils/coll->string (:header datagen-output)) :append true)
                                                 (spit datagen-file (utils/coll->string (:data datagen-output)) :append true)))
    (if (and eod? (not eod-plus-one?))
      (do

        (when (not (zero? current-position))
          (reset! (:position-status agent) 0)
          (reset! (:position agent) 0)
          (reset! (:eod-position agent) current-position)
          (swap! (:n-trades agent) inc)
          (if (pos? current-position) (swap! (:cash-account agent) + (* current-position-abs bidprice)) (swap! (:cash-account agent) - (* current-position-abs askprice)))
          (reset! (:allowed-to-enter-long? agent) false)
          (reset! (:allowed-to-enter-short? agent) false)
          (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))

          (if (pos? current-position)
            (do (if live? (execution/SELL ib-tic current-position-abs))
                (timbre/debug (red "EODSELLCLOSE " " tic " ib-tic " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl   " total-pnl " @(:total-pnl agent))))
            (do (if live? (execution/BUY ib-tic current-position-abs))
                (timbre/debug (green "EODBUYCLOSE " " tic " ib-tic " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl   " total-pnl " @(:total-pnl agent)))))
          (reset! (:m2m-pnl agent) @(:cash-account agent))
;; eod pnl can be max pnl or min
          (let [pnl @(:m2m-pnl agent)]
            (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
            (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl))))

        (reset! (@((indicator-state tic) date) :eod-plus-one?)  true) ;; marking eod process complete and end of trading
        (reset! (@((indicator-state tic) date) :pnl) @(:total-pnl agent)))   ;; end EOD stuff


;; if not EOD
      (when-not eod?   ;;;  this is because of eod-plus-one flag becomes true at last step

  ;; heres where the otl is reset when we can trade again
        (when (>= time-since-last-otl-msecs otl-sleep)
          (reset! (:allowed-to-enter-long? agent) true)
          (reset! (:allowed-to-enter-short? agent) true)
          (reset! (:otl-start-time agent) Double/NaN))

    ;; if i hit $300 total pnl and have lost $50, go home with 250 stop trading!
        (when ;false
         total-drawdown-breached?   ;; disable for now
          (reset! (@((indicator-state tic) date) :eod?) true)
          (reset! (:allowed-to-enter-long? agent) false)
          (reset! (:allowed-to-enter-short? agent) false)
          (timbre/debug (red "GO-HOME-ON-DRAW!!! " " tic " ib-tic " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)  " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)    " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread))
;;raise eod flag and end trading
)  ;;; when otl


        (when-not (zero? position-diff-abs)  ;;some positions to be taken/eliminated
;;can be removed
     ;; end when zero target


   ;; get long (from short or from flat)
   ;; else

          (when (and (pos? target-position) allowed-to-enter-long?
                ;; allowed to enter long
                     @(:allowed-to-enter-long? agent))
            (swap! (:position agent) + unsigned-actual-trade-size)
            (reset! (:position-status agent) target-position)
            (swap! (:last-entry-price agent) conj askprice)
            (reset! (:last-entry-time agent) time-now)
            (swap! (:cash-account agent) - (* unsigned-actual-trade-size askprice))
            (reset! (:max-otl agent) tentative-max-otl)
            (reset! (:last-entry-time agent) time-now)
                                        ;(execution/buy-hedge-new tic unsigned-actual-trade-size unsigned-market-trade-size  )
            (if live? (execution/BUY ib-tic unsigned-actual-trade-size))
            (timbre/debug (green "BUY " " tic " ib-tic " time-string " time-string " time " time  " price " askprice " somany " unsigned-actual-trade-size " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl " total-pnl " @(:total-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent) " draw " draw " spread " spread  " max-otl " @(:max-otl agent)))
            (swap! (:n-trades agent) inc)
            (let [current-position @(agent :position)
                  current-position-abs (incanter/abs current-position)
                 ;;correct pnl accounting, if long i can get out on bid only
                 ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )
                  value-of-positions (* reference-price current-position)
                  net-value (+ @(agent :cash-account)  value-of-positions)]

              (reset! (:m2m-pnl agent) net-value)
              (if (>= net-value @(:max-pnl agent)) (reset! (:max-pnl agent) net-value))
              (if (<= net-value @(:min-pnl agent)) (reset! (:min-pnl agent) net-value))
              (timbre/debug (yellow "M2M " " tic " ib-tic " time " time  " price " reference-price " current-position " current-position " n-trades " current-n-trades " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " total-pnl " @(:total-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)  " draw " draw " spread " spread))))

         ;; get long (from short or from flat)
          (when (and (neg? target-position) allowed-to-enter-short?
                     @(:allowed-to-enter-short? agent))
            (swap! (:position agent) - unsigned-actual-trade-size)
            (reset! (:position-status agent) target-position)
            (swap! (:last-entry-price agent) conj askprice)
            (reset! (:last-entry-time agent) time-now)
            (swap! (:cash-account agent) + (* unsigned-actual-trade-size bidprice))
            (reset! (:max-otl agent) tentative-max-otl)
            (reset! (:last-entry-time agent) time-now)
                                        ;(execution/sell-hedge-new tic unsigned-actual-trade-size unsigned-market-trade-size  )
            (if live? (execution/SELL ib-tic unsigned-actual-trade-size))
            (timbre/debug (red "SELL " " tic " ib-tic " time-string " time-string " time " time  " price " bidprice " somany " unsigned-actual-trade-size " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl " total-pnl " @(:total-pnl agent)  " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent) " draw " draw " spread " spread  " max-otl " @(:max-otl agent)))
            (swap! (:n-trades agent) inc)
            (let [current-position @(agent :position)
                  current-position-abs (incanter/abs current-position)
                 ;;correct pnl accounting, if long i can get out on bid only
                 ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )
                  value-of-positions (* reference-price current-position)
                  net-value (+ @(agent :cash-account)  value-of-positions)]

              (reset! (:m2m-pnl agent) net-value)
              (if (>= net-value @(:max-pnl agent)) (reset! (:max-pnl agent) net-value))
              (if (<= net-value @(:min-pnl agent)) (reset! (:min-pnl agent) net-value))
              (timbre/debug (yellow "M2M " " tic " ib-tic " time " time  " price " reference-price " current-position " current-position " n-trades " current-n-trades " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " total-pnl " @(:total-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)  " draw " draw " spread " spread)))))
  ;;; m2m pnl update when not doing anythin
        (when (zero? position-diff-abs) (let [current-position @(agent :position)
                                              current-position-abs (incanter/abs current-position)
                   ;;correct pnl accounting, if long i can get out on bid only
                   ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )
                                              value-of-positions (* reference-price current-position)
                                              net-value (utils/round (+ @(agent :cash-account)  value-of-positions))]

                                          (reset! (:m2m-pnl agent) net-value)
                                          (if (>= net-value @(:max-pnl agent)) (reset! (:max-pnl agent) net-value))
                                          (if (<= net-value @(:min-pnl agent)) (reset! (:min-pnl agent) net-value))
;(timbre/debug (cyan "M2M " " tic " ib-tic " time " time  " price " reference-price " current-position " current-position " n-trades " current-n-trades " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " total-pnl " @(:total-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent) " draw " draw  ))
)

;; only exit mechanism now is when not eod, if dd breached or otl breached
              (when (and (not @(@((indicator-state tic) date) :eod?))
                         (or
                                        ;getflat?
                          false
                     ;otl-breached?
                     ;drawdown-breached?
                     ;ttc-breached?
))
                (cond
                  (pos? current-position)
                  (do
                    (swap! (:position agent) - current-position-abs)
                    (reset! (:position-status agent) 0.0)
                    (swap! (:last-exit-price agent) conj price)

                                        ;(reset! (:max-pnl-this-round agent) 0.0)
                    (reset! (:last-entry-time agent) nil)
                    (swap! (:n-trades agent) inc)
                    (swap! (:cash-account agent) + (* current-position-abs bidprice))
                                        ;(reset! (:m2m-pnl agent)  @(:cash-account agent)  ) ;;; this is round pnl (total pnl is sum of rounds)
                                        ;(swap! (:total-pnl agent) + (utils/round @(:cash-account agent)    )  )
                    (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))
                    (if (> @(:total-pnl agent) @(:max-total-pnl agent)) (reset! (:max-total-pnl agent)  @(:total-pnl agent)))

                    (if live? (execution/SELL ib-tic current-position-abs))
                    (timbre/debug (on-white (red "SELL-CLOSE-ON-DRAW " " tic " ib-tic " time-string " time-string " time " time " price " bidprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)   " total-pnl " @(:total-pnl agent) " draw " draw  " allowed-drawdown " allowed-drawdown " spread " spread)))

                    (when
                     otl-breached?
                      (swap! (agent :n-otls) inc)                                                                                                   (timbre/debug (magenta "OTL-BREACH " " tic " ib-tic " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)   " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread  " max-otl " @(:max-otl agent))))
                    (when
                     getflat?
                      (timbre/debug (on-yellow (blue "GETFLAT " " tic " ib-tic " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)   " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread  " max-otl " @(:max-otl agent)))))

                    (reset! (:allowed-to-enter-long? agent) false)
                    (reset! (:allowed-to-enter-short? agent) false)
                    (reset! (:otl-start-time agent) time-now))
                  (neg? current-position)
                  (do
                    (swap! (:position agent) + current-position-abs)
                    (reset! (:position-status agent) 0.0)
                    (swap! (:last-exit-price agent) conj price)

                                        ;(reset! (:m2m-pnl-bps agent) 0.0)
                                        ;(reset! (:max-pnl-this-round agent) 0.0)
                    (reset! (:last-entry-time agent) nil)
                    (swap! (:n-trades agent) inc)
                    (swap! (:cash-account agent) - (* current-position-abs askprice))
                                        ;(reset! (:m2m-pnl agent)  @(:cash-account agent)  )
                    (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))
                    (if (> @(:total-pnl agent) @(:max-total-pnl agent)) (reset! (:max-total-pnl agent)  @(:total-pnl agent)))
                    (if live? (execution/BUY ib-tic current-position-abs))
                    (timbre/debug (blue "BUY-CLOSE-ON-DRAW " " tic " ib-tic " time-string " time-string " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)  " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)    " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread))

                    (when otl-breached?
                      (swap! (agent :n-otls) inc)                                                                         (timbre/debug (magenta "OTL-BREACH " " tic " ib-tic " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)   " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread  " max-otl " @(:max-otl agent))))

                    (when
                     getflat?
                      (timbre/debug (on-yellow (blue "GETFLAT " " tic " ib-tic " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)   " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread  " max-otl " @(:max-otl agent))))) (reset! (:allowed-to-enter-long? agent) false)
                    (reset! (:allowed-to-enter-short? agent) false)
                    (reset! (:otl-start-time agent) time-now)))

                (reset! (:cash-account agent) 0.0)  ;; finished one round of trading
                (reset! (:m2m-pnl agent) 0.0)  ;; finished one round of trading
                (reset! (:max-pnl agent) (- 99.0)) ;; finished one round of trading
                (reset! (:min-pnl agent) 99.0)  ;; finished one round of trading
)  ;;; when otl
)
        (reset! (@((indicator-state tic) date) :pnl) @(:total-pnl agent))))   ;; if eod
))

(defn trading-logic-position-target-limit
  "
core function, called only when bar data is updated.
strong signal -> target position is positive
weak signal -> exit
strong short signal -> target position is negative
weak signal -> exit

"
  [log-file-name datagen-file datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        price-bars @(@((indicator-state tic) date) :price-bars)
                                        ;sigma (incanter/sd price-bars)

        ;price-band [ (utils/round (- price sigma)) (utils/round (+ price sigma)) ]
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)
        spread (utils/round (- askprice bidprice)) target-position-prior-with-signal ((:target-position agent) indicator-state tic date)
        target-position-prior (first target-position-prior-with-signal)
        signal-state (second target-position-prior-with-signal) exit-position ((:exit-position agent) indicator-state tic date)
        ;reference-price ( (:reference-price agent) indicator-state tic date) ;; only units 0 -> 5 -> -5

        capital 10000.0
        max-uts (:data (indicators/position-to-take capital indicator-state tic date)) ;; the jump from 0 is this position

        calc-uts ((:target-size agent) indicator-state tic date) ;; can be NaN
        uts (apply min [max-uts calc-uts])
        ;market-uts (:data (indicators/position-to-take capital indicator-state :SPY date)) ;; the jump from 0 is this position
        current-position @(agent :position)
        current-position-abs (incanter/abs current-position)
        current-position-status @(agent :position-status)  ;; this is + 1 -1 or 0 ; long short or flat
        getflat? (and (not (zero? exit-position)) (not (zero? current-position-status)) (not= current-position-status exit-position))
        ;target-position (cond (neg? (* current-position target-position-prior)) 0.0 :else target-position-prior )
        ;; if we have position ignore signal and use drawdowns to exit (or ttc) - a hack
        target-position (cond (not (zero? current-position)) current-position-status :else target-position-prior)  ;;t-p is always a unit quantity

        position-diff (cond (Double/isNaN target-position) 0.0 :else (- target-position current-position-status))
        position-diff-abs (incanter/abs position-diff) unsigned-actual-trade-size (* uts position-diff-abs)  ;; typically 1 or 2 times uts, signed quantity ; causes residual positions for now fix later
        ;unsigned-market-trade-size (* market-uts position-diff-abs)
        current-n-trades @(agent :n-trades)
        current-n-otls @(agent :n-otls)

;;correct pnl accounting, if long i can get out on bid only
        ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )

        agent-name (agent :agent-name) time-now @timekeeper/global-time-now
        time-string @(@((indicator-state tic) date) :time-string)
        ;max-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps
        total-pnl @(:total-pnl agent) ;; this is in dollars now

        eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.5
        first-datapoint? (not (.exists (io/as-file datagen-file))) datagen-output (cond (nil? (:datagen agent)) nil
                                                                                        :else ((:datagen agent) indicator-state tic date))
        live? (cond (nil? @(:live? agent)) false :else @(:live? agent)) ;; place orders
        time @(:seq-no @((indicator-state tic) date))
        ib-tic (name tic)
        cash-account @(:cash-account agent) ;; this is in bps

        m2m-pnl @(:m2m-pnl agent)
        max-pnl @(:max-pnl agent)
        draw (- max-pnl m2m-pnl)
        allowed-drawdown (utils/drawdown-at-pnl (apply max [max-pnl total-pnl]))
        drawdown-breached? (> draw allowed-drawdown)
        tentative-max-otl 100.0
        ;(apply max [ ( (:max-otl2 agent) indicator-state tic date) 100.0  ])
        allowed-to-enter-long? (and (not= tic :SPY) (not (Double/isNaN uts)) (not (Double/isNaN tentative-max-otl)) @(:allowed-to-enter-long? agent) (< current-n-trades (agent :max-n-trades)) (< @(agent :n-otls) (agent :max-otls))   (<= spread (agent :max-spread)))

        allowed-to-enter-short? (and (not= tic :SPY) (not (Double/isNaN uts)) (not (Double/isNaN tentative-max-otl)) @(:allowed-to-enter-short? agent) (< current-n-trades (agent :max-n-trades))  (< @(agent :n-otls) (agent :max-otls))   (<= spread (agent :max-spread)))
;;correct pnl accounting, if long i can get out on bid only
        ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )
        reference-price ((:reference-price agent) indicator-state tic date) otl-breached? (>= (- m2m-pnl) @(:max-otl agent))
        otl-sleep (* 60000 (:break-after-otl-minutes agent))
        time-since-last-otl-msecs (- time-now @(:otl-start-time agent));; now i take care of max-total-pnl, where a good thing is preserved
        max-total-pnl @(:max-total-pnl agent)

        ;m2m-pnl-relevant (cond (neg? m2m-pnl) (- m2m-pnl) :else 0) ;; only if negative i worry
        go-home-pnl (+ total-pnl m2m-pnl) ;; this is my total-pnl-draw
        total-pnl-draw (- max-total-pnl go-home-pnl)
        total-allowed-drawdown (utils/drawdown-at-pnl max-total-pnl)
        total-drawdown-breached? (> total-pnl-draw total-allowed-drawdown)
        last-entry-time  (cond (zero? @(tic trackorders/order-fill-time)) Double/NaN :else  @(tic trackorders/order-fill-time))
        ttc-now-minutes (cond (zero? current-position) 0.0 :else (utils/round (/ (- time-now last-entry-time) 60000)))
        ttc-breached? (cond (nil? ttc-now-minutes) false :else (>= ttc-now-minutes (:max-ttc-minutes agent)))
        actual-position (incanter/abs  @(tic trackorders/order-inventory))
        actual-signed-position @(tic trackorders/order-inventory)]
    ;;remember eod positions and total-pnls are not correct in datagen bcos
    ;; after eod it does not print another point

    (if (and eod? (not eod-plus-one?))
      ;; if EOD close with market orders - fix size I have now, this is not current pos
      (do

        (when (not (zero? actual-position))
          (reset! (:position-status agent) 0)
          (reset! (:position agent) 0)
          (reset! (:eod-position agent) current-position)
          (swap! (:n-trades agent) inc)
          (if (pos? actual-signed-position) (swap! (:cash-account agent) + (* current-position-abs bidprice)) (swap! (:cash-account agent) - (* current-position-abs askprice)))
          (reset! (:allowed-to-enter-long? agent) false)
          (reset! (:allowed-to-enter-short? agent) false)
          (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))

          (if (pos? actual-signed-position)
            (do (if live? (execution/SELL ib-tic actual-position))
                (timbre/debug (red "EODSELLCLOSE " " tic " ib-tic " price " askprice " current-position " actual-signed-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl   " total-pnl " @(:total-pnl agent))))
            (do (if live? (execution/BUY ib-tic actual-position))
                (timbre/debug (green "EODBUYCLOSE " " tic " ib-tic " price " askprice " current-position " actual-signed-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl   " total-pnl " @(:total-pnl agent)))))
          (reset! (:m2m-pnl agent) @(:cash-account agent))
;; eod pnl can be max pnl or min
          (let [pnl @(:m2m-pnl agent)]
            (if (> pnl @(:max-pnl agent)) (reset! (:max-pnl agent) pnl))
            (if (< pnl @(:min-pnl agent)) (reset! (:min-pnl agent) pnl))))

        (reset! (@((indicator-state tic) date) :eod-plus-one?)  true) ;; marking eod process complete and end of trading
        (reset! (@((indicator-state tic) date) :pnl) @(:total-pnl agent)))   ;; end EOD stuff


;; if not EOD
      (when-not eod?   ;;;  this is because of eod-plus-one flag becomes true at last step
        (when-not (zero? position-diff-abs)  ;;some positions to be taken/eliminated
;; this should be a limit order
          (when (and (pos? target-position) allowed-to-enter-long?

                ;; allowed to enter long
                     @(:allowed-to-enter-long? agent)
               ;; if high-imp true, i dont send orders! Just wait.
                     (not @((keyword ib-tic) trackorders/high-impedance)))
            (swap! (:position agent) + 100)
            (reset! (:position-status agent) target-position)
            (swap! (:last-entry-price agent) conj askprice)
            (reset! (:last-entry-time agent) time-now)
            (swap! (:cash-account agent) - (* 100 askprice))
            (reset! (:max-otl agent) tentative-max-otl)
            (reset! (:last-entry-time agent) time-now)
                                        ;(execution/buy-hedge-new tic unsigned-actual-trade-size unsigned-market-trade-size  )
            (if live? (execution/BUY-LIMIT ib-tic (utils/round2 2 bidprice)  100))
           ;; now you wait for order confirmation
            (reset! ((keyword ib-tic) trackorders/high-impedance) true)

           ;; set high impedance here, cant do anything when i wait
            (timbre/debug (green "BUY-LIMIT " " tic " ib-tic " time-string " time-string " time " time  " price " bidprice " somany " 100 " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl " total-pnl " @(:total-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent) " draw " draw " spread " spread  " max-otl " @(:max-otl agent)  " signal-state " signal-state))
            (swap! (:n-trades agent) inc)
            (let [current-position @(agent :position)
                  current-position-abs (incanter/abs current-position)
                 ;;correct pnl accounting, if long i can get out on bid only
                 ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )
                  value-of-positions (* reference-price current-position)
                  net-value (+ @(agent :cash-account)  value-of-positions)]

              (reset! (:m2m-pnl agent) net-value)
              (if (>= net-value @(:max-pnl agent)) (reset! (:max-pnl agent) net-value))
              (if (<= net-value @(:min-pnl agent)) (reset! (:min-pnl agent) net-value))
              (timbre/debug (yellow "M2M " " tic " ib-tic " time " time  " price " reference-price " current-position " current-position " n-trades " current-n-trades " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " total-pnl " @(:total-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)  " draw " draw " spread " spread))))

         ;; again limit order here
          (when (and (neg? target-position) allowed-to-enter-short?
                     @(:allowed-to-enter-short? agent)
                     (not @((keyword ib-tic) trackorders/high-impedance)))
            (swap! (:position agent) - 100)
            (reset! (:position-status agent) target-position)
            (swap! (:last-entry-price agent) conj askprice)
            (reset! (:last-entry-time agent) time-now)
            (swap! (:cash-account agent) + (* 100 bidprice))
            (reset! (:max-otl agent) tentative-max-otl)
            (reset! (:last-entry-time agent) time-now)
                                        ;(execution/sell-hedge-new tic unsigned-actual-trade-size unsigned-market-trade-size  )
            (if live? (execution/SELL-LIMIT ib-tic (utils/round2 2 askprice) 100))
            (reset! ((keyword ib-tic) trackorders/high-impedance) true)

            (timbre/debug (red "SELL-LIMIT " " tic " ib-tic " time-string " time-string " time " time  " price " bidprice " somany " unsigned-actual-trade-size " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " m2m-pnl " total-pnl " @(:total-pnl agent)  " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent) " draw " draw " spread " spread  " max-otl " @(:max-otl agent)  " signal-state " signal-state))
            (swap! (:n-trades agent) inc)
            (let [current-position @(agent :position)
                  current-position-abs (incanter/abs current-position)
                 ;;correct pnl accounting, if long i can get out on bid only
                 ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )
                  value-of-positions (* reference-price current-position)
                  net-value (+ @(agent :cash-account)  value-of-positions)]

              (reset! (:m2m-pnl agent) net-value)
              (if (>= net-value @(:max-pnl agent)) (reset! (:max-pnl agent) net-value))
              (if (<= net-value @(:min-pnl agent)) (reset! (:min-pnl agent) net-value))
              (timbre/debug (yellow "M2M " " tic " ib-tic " time " time  " price " reference-price " current-position " current-position " n-trades " current-n-trades " cash-account " @(:cash-account agent) " m2m-pnl " @(:m2m-pnl agent) " total-pnl " @(:total-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)  " draw " draw " spread " spread)))))
  ;;; m2m pnl update when not doing anythin
        (if (not ttc-breached?)
          (when (pos? (incanter/abs  @(tic trackorders/order-inventory)))
                        ;; only exit mechanism now is when not eod, try to exit at best price
            (cond
              (and (pos? @(tic trackorders/order-inventory))
                   (> (*  @(tic trackorders/order-inventory) (- askprice @(tic trackorders/order-fills))) 3)

                   (not @((keyword ib-tic) trackorders/high-impedance)))
              (do
                (swap! (:position agent) - current-position-abs)
                (reset! (:position-status agent) 0.0)
                (swap! (:last-exit-price agent) conj price)

                                        ;(reset! (:max-pnl-this-round agent) 0.0)
                (reset! (:last-entry-time agent) nil)
                (swap! (:n-trades agent) inc)
                (swap! (:cash-account agent) + (* current-position-abs bidprice))
                                        ;(reset! (:m2m-pnl agent)  @(:cash-account agent)  ) ;;; this is round pnl (total pnl is sum of rounds)
                                        ;(swap! (:total-pnl agent) + (utils/round @(:cash-account agent)    )  )
                (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))
                (if (> @(:total-pnl agent) @(:max-total-pnl agent)) (reset! (:max-total-pnl agent)  @(:total-pnl agent)))

                (if live? (execution/SELL-LIMIT ib-tic (utils/round2 2 askprice) (incanter/abs @(tic trackorders/order-inventory))))
                (reset! ((keyword ib-tic) trackorders/high-impedance) true)

                (timbre/debug (on-white (red "SELL-CLOSE-PASSIVE " " tic " ib-tic " time-string " time-string " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)   " total-pnl " @(:total-pnl agent) " draw " draw  " allowed-drawdown " allowed-drawdown " spread " spread))) (reset! (:allowed-to-enter-long? agent) false)
                (reset! (:allowed-to-enter-short? agent) false)
                (reset! (:otl-start-time agent) time-now))
              (and (neg?  @(tic trackorders/order-inventory))
                   (>  @(tic trackorders/order-fills) bidprice)

                   (> (*  @(tic trackorders/order-inventory) (- bidprice @(tic trackorders/order-fills))) 3)

                   (not @((keyword ib-tic) trackorders/high-impedance)))

              (do
                (swap! (:position agent) + current-position-abs)
                (reset! (:position-status agent) 0.0)
                (swap! (:last-exit-price agent) conj price)

                (reset! (:last-entry-time agent) nil)
                (swap! (:n-trades agent) inc)
                (swap! (:cash-account agent) - (* current-position-abs askprice))
                                        ;(reset! (:m2m-pnl agent)  @(:cash-account agent)  )
                (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))
                (if (> @(:total-pnl agent) @(:max-total-pnl agent)) (reset! (:max-total-pnl agent)  @(:total-pnl agent)))

                (if live? (execution/BUY-LIMIT ib-tic (utils/round2 2 bidprice) (incanter/abs @(tic trackorders/order-inventory))))
                (reset! ((keyword ib-tic) trackorders/high-impedance) true)

                (timbre/debug (blue "BUY-CLOSE-PASSIVE " " tic " ib-tic " time-string " time-string " time " time " price " bidprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)  " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)    " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread))

                (reset! (:allowed-to-enter-long? agent) false)
                (reset! (:allowed-to-enter-short? agent) false)
                (reset! (:otl-start-time agent) time-now)))

            (reset! (:cash-account agent) 0.0) ;; finished one round of trading
            (reset! (:m2m-pnl agent) 0.0) ;; finished one round of trading
            (reset! (:max-pnl agent) (- 99.0)) ;; finished one round of trading
            (reset! (:min-pnl agent) 99.0) ;; finished one round of trading


;;; when otl
)

      ;;if ttc breached exit aggress

          (when (pos? (incanter/abs  @(tic trackorders/order-inventory)))
                        ;; only exit mechanism now is when not eod, try to exit at best price
            (cond
              (and (pos? @(tic trackorders/order-inventory)) (not @((keyword ib-tic) trackorders/high-impedance)))
              (do
                (swap! (:position agent) - current-position-abs)
                (reset! (:position-status agent) 0.0)
                (swap! (:last-exit-price agent) conj price)

                                        ;(reset! (:max-pnl-this-round agent) 0.0)
                (reset! (:last-entry-time agent) nil)
                (swap! (:n-trades agent) inc)
                (swap! (:cash-account agent) + (* current-position-abs bidprice))
                                        ;(reset! (:m2m-pnl agent)  @(:cash-account agent)  ) ;;; this is round pnl (total pnl is sum of rounds)
                                        ;(swap! (:total-pnl agent) + (utils/round @(:cash-account agent)    )  )
                (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))
                (if (> @(:total-pnl agent) @(:max-total-pnl agent)) (reset! (:max-total-pnl agent)  @(:total-pnl agent)))

                (if live? (execution/SELL ib-tic (incanter/abs @(tic trackorders/order-inventory))))
                (reset! ((keyword ib-tic) trackorders/high-impedance) true)

                (timbre/debug (on-white (red "SELL-CLOSE-AGG " " tic " ib-tic " time-string " time-string " time " time " price " askprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent) " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)   " total-pnl " @(:total-pnl agent) " draw " draw  " allowed-drawdown " allowed-drawdown " spread " spread))) (reset! (:allowed-to-enter-long? agent) false)
                (reset! (:allowed-to-enter-short? agent) false)
                (reset! (:otl-start-time agent) time-now))
              (and (neg?  @(tic trackorders/order-inventory))
                   (>  @(tic trackorders/order-fills) bidprice) (not @((keyword ib-tic) trackorders/high-impedance)))

              (do
                (swap! (:position agent) + current-position-abs)
                (reset! (:position-status agent) 0.0)
                (swap! (:last-exit-price agent) conj price)

                (reset! (:last-entry-time agent) nil)
                (swap! (:n-trades agent) inc)
                (swap! (:cash-account agent) - (* current-position-abs askprice))
                                        ;(reset! (:m2m-pnl agent)  @(:cash-account agent)  )
                (swap! (:total-pnl agent) + (utils/round (- @(:cash-account agent) (* commission 2))))
                (if (> @(:total-pnl agent) @(:max-total-pnl agent)) (reset! (:max-total-pnl agent)  @(:total-pnl agent)))

                (if live? (execution/BUY ib-tic (incanter/abs @(tic trackorders/order-inventory))))
                (reset! ((keyword ib-tic) trackorders/high-impedance) true)

                (timbre/debug (blue "BUY-CLOSE-AGG " " tic " ib-tic " time-string " time-string " time " time " price " bidprice " current-position " current-position " n-trades " current-n-trades " cash-account " cash-account " m2m-pnl " @(:m2m-pnl agent)  " max-pnl " @(:max-pnl agent) " min-pnl " @(:min-pnl agent)    " total-pnl " @(:total-pnl agent) " draw " draw " allowed-drawdown " allowed-drawdown " spread " spread))

                (reset! (:allowed-to-enter-long? agent) false)
                (reset! (:allowed-to-enter-short? agent) false)
                (reset! (:otl-start-time agent) time-now)))

            (reset! (:cash-account agent) 0.0) ;; finished one round of trading
            (reset! (:m2m-pnl agent) 0.0) ;; finished one round of trading
            (reset! (:max-pnl agent) (- 99.0)) ;; finished one round of trading
            (reset! (:min-pnl agent) 99.0) ;; finished one round of trading


;;; when otl
))
        (reset! (@((indicator-state tic) date) :pnl) @(:total-pnl agent))))   ;; if eod
));;; when otl


(defn trading-logic-debug
  "
core function, called only when bar data is updated.
strong signal -> target position is positive
weak signal -> exit
strong short signal -> target position is negative
weak signal -> exit

"
  [log-file-name datagen-file datagen? agent indicator-state tic date]

  ;;evaluate long entry
  (let [price @(@((indicator-state tic) date) :price)
        price-bars @(@((indicator-state tic) date) :price-bars)
                                        ;sigma (incanter/sd price-bars)

        ;price-band [ (utils/round (- price sigma)) (utils/round (+ price sigma)) ]
        bidprice @(@((indicator-state tic) date) :bidprice)
        askprice @(@((indicator-state tic) date) :askprice)
        spread (utils/round (- askprice bidprice))

        target-position-prior-with-signal ((:target-position agent) indicator-state tic date)
        target-position-prior (first target-position-prior-with-signal)
        signal-state (second target-position-prior-with-signal)

        exit-position ((:exit-position agent) indicator-state tic date)
        ;reference-price ( (:reference-price agent) indicator-state tic date) ;; only units 0 -> 5 -> -5

        capital 10000.0
        max-uts (:data (indicators/position-to-take capital indicator-state tic date)) ;; the jump from 0 is this position

        calc-uts ((:target-size agent) indicator-state tic date) ;; can be NaN
        uts (apply min [max-uts calc-uts])
        ;market-uts (:data (indicators/position-to-take capital indicator-state :SPY date)) ;; the jump from 0 is this position
        current-position @(agent :position)
        current-position-abs (incanter/abs current-position)
        current-position-status @(agent :position-status)  ;; this is + 1 -1 or 0 ; long short or flat
        getflat? (and (not (zero? exit-position)) (not (zero? current-position-status)) (not= current-position-status exit-position))
        ;target-position (cond (neg? (* current-position target-position-prior)) 0.0 :else target-position-prior )
        ;; if we have position ignore signal and use drawdowns to exit (or ttc) - a hack
        target-position (cond (not (zero? current-position)) current-position-status :else target-position-prior)  ;;t-p is always a unit quantity

        position-diff (cond (Double/isNaN target-position) 0.0 :else (- target-position current-position-status))
        position-diff-abs (incanter/abs position-diff) unsigned-actual-trade-size (* uts position-diff-abs)  ;; typically 1 or 2 times uts, signed quantity ; causes residual positions for now fix later
        ;unsigned-market-trade-size (* market-uts position-diff-abs)
        current-n-trades @(agent :n-trades)

;;correct pnl accounting, if long i can get out on bid only
        ;reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price )

        agent-name (agent :agent-name) time-now @(@((indicator-state tic) date) :time)
        time-string @(@((indicator-state tic) date) :time-string)
        ;max-pnl-this-round @(:max-pnl-this-round agent) ;; this is in bps
        total-pnl @(:total-pnl agent) ;; this is in dollars now

        eod? @(@((indicator-state tic) date) :eod?)
        eod-plus-one? @(@((indicator-state tic) date) :eod-plus-one?)

        commission 1.5
        first-datapoint? (not (.exists (io/as-file datagen-file))) datagen-output (cond (nil? (:datagen agent)) nil
                                                                                        :else ((:datagen agent) indicator-state tic date))
        live? (cond (nil? @(:live? agent)) false :else @(:live? agent)) ;; place orders
        time @(:seq-no @((indicator-state tic) date))
        ib-tic (name tic)
        cash-account @(:cash-account agent) ;; this is in bps

        m2m-pnl @(:m2m-pnl agent)
        max-pnl @(:max-pnl agent)
        draw (- max-pnl m2m-pnl)
        allowed-drawdown (utils/drawdown-at-pnl (apply max [max-pnl total-pnl]))
        drawdown-breached? (> draw allowed-drawdown)
        tentative-max-otl 100.0
        ;(apply max [ ( (:max-otl2 agent) indicator-state tic date) 100.0  ])
        allowed-to-enter-long? (and (not (Double/isNaN uts)) (not (Double/isNaN tentative-max-otl)) @(:allowed-to-enter-long? agent) (< current-n-trades (agent :max-n-trades)))

        allowed-to-enter-short? (and (not (Double/isNaN uts)) (not (Double/isNaN tentative-max-otl)) @(:allowed-to-enter-short? agent) (< current-n-trades (agent :max-n-trades)))
;;correct pnl accounting, if long i can get out on bid only
        reference-price (cond (pos? current-position) askprice (neg? current-position) bidprice :else price) otl-breached? (>= (- m2m-pnl) @(:max-otl agent))
        otl-sleep (* 60000 (:break-after-otl-minutes agent))
        time-since-last-otl-msecs (- time-now @(:otl-start-time agent));; now i take care of max-total-pnl, where a good thing is preserved
        max-total-pnl @(:max-total-pnl agent)
        ;m2m-pnl-relevant (cond (neg? m2m-pnl) (- m2m-pnl) :else 0) ;; only if negative i worry
        go-home-pnl (+ total-pnl m2m-pnl) ;; this is my total-pnl-draw
        total-pnl-draw (- max-total-pnl go-home-pnl)
        total-allowed-drawdown (utils/drawdown-at-pnl max-total-pnl)
        total-drawdown-breached? (> total-pnl-draw total-allowed-drawdown)]
    ;;remember eod positions and total-pnls are not correct in datagen bcos
    ;; after eod it does not print another point
    (pprint "Hi")
    (comment first-datapoint?
             (pprint (utils/coll->string (:header datagen-output)))
             (pprint (utils/coll->string (:data datagen-output))))
   ;; if eod
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

        datagen-file (str utils/HOME "/data/" (str/lower-case ib-tic) "." (name date)  ".us.txt")

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
