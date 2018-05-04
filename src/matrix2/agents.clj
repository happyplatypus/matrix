(ns matrix.agents (:require
                   [matrix.utils :as utils]
                   [matrix.indicators :as indicators]
                   [matrix.jutsudata :as jutsudata]
                   [matrix.tickers :as tickers]
                   [clojure.java.io :as io]
                   [clojure.string :as str]
                   [clj-http.client :as client]
                   [clj-time.format :as tf]
                   [clj-time.core :as tt]
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

                   [trees.models :as models]
                   [trees.dostuff :refer [learn classify]] [matrix.tickers :as tickers])

    (:use clojure.pprint))

;(identity models/iris-tree-model1)


(defn agent-1
  "simple strat that enters here exits there, no parameters"
  []
  (atom {;; dispersion larger than 100 bps
      ;; dispersion not constant
      ;; ttc exit
         ;; ema diff positive
         ;;consolidated signal to [long-entry short-entry] booleans

         :entry
         #(let [indicator-state %1
                tic %2
                date %3

                i5 (:data (indicators/price-ema 5 indicator-state tic date))
                i6 (:data (indicators/price-ema 26 indicator-state tic date))

                i8 (:data (indicators/moving-dispersion indicator-state tic date))

                dispersion @(:dispersion @((indicator-state tic) date))]
            [(and
              (> dispersion 100.0)
              (not (zero? i8))

              (pos? (- i5 i6)))
             (and
              (> dispersion 100.0)
              (not (zero? i8))

              (neg? (- i5 i6)))])

         :pnl-target-bps 50
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps nil
         :n-trades (atom 0.0)
         :max-n-trades 1.0 ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name "Trooper"
         :max-ttc-minutes 40.0
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           i1 (indicators/returns-from-open indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema 5 indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                           i10 (indicators/returns-quality indicator-state tic date) dispersion @(:dispersion @((indicator-state tic) date)) price @(:price @((indicator-state tic) date))
                           time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                       {:header
                        ["date" "time" "tic" "price" (:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i9) (:header i10)]
                        :data [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i9) (:data i10)]})}))

(defn agent-2
  "complex strat that enters here exits there, with parameters"
  [rr stdev pnl-target]

  (atom {:long-entry
         #(let [indicator-state %1
                tic %2
                date %3] (and (> (:data (indicators/recent-returns indicator-state tic date)) rr)
                              (< (:data (indicators/stdev-ema indicator-state tic date)) stdev)))
         :short-entry
         #(let [indicator-state %1
                tic %2
                date %3] (and (:data (< (indicators/recent-returns indicator-state tic date) (- rr)))
                              (< (:data (indicators/stdev-ema indicator-state tic date)) stdev)))
         :pnl-target pnl-target
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps 50.00}))

(defn agent-3
  "complex strat that enters here exits there, with parameters"
  [D zpp draw max-ttc rq]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3
                i8 (:data (indicators/moving-dispersion indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))
                i5 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i6 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                i7 (:data (indicators/returns-quality indicator-state tic date))]
            [(and
              (> dispersion D)
              (not (zero? i8))
              (>= (- i5 i6) zpp)
              (>= i7 rq)  ;; returns quality threshold
)
             (and
              (> dispersion D)
              (not (zero? i8))
              (<= (- i5 i6) (- zpp))
              (<= i7 (- rq)) ;; returns quality threshold
)])
         :pnl-target-bps nil
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps draw
         :n-trades (atom 0.0)
         :max-n-trades 10.0 ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes max-ttc
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           i1 (indicators/returns-from-open indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                           i10 (indicators/returns-quality indicator-state tic date)
                           dispersion @(:dispersion @((indicator-state tic) date))
                           price @(:price @((indicator-state tic) date))
                           time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                       {:header
                        ["date" "time" "tic" "price" (:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i9) (:header i10)]

                        :data
                        [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i9) (:data i10)]})}))

(defn agent-4-profit-taker
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [zpp rq pnl-target-bps max-n-trades]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3
                i8 (:data (indicators/moving-dispersion indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))
                i5 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i6 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                i7 (:data (indicators/returns-quality indicator-state tic date))]
            [(and
              (> dispersion 100)
              (not (zero? i8))
              (>= (- i5 i6) zpp)
              (>= i7 rq)  ;; returns quality threshold
)
             (and
              (> dispersion 100)
              (not (zero? i8))
              (<= (- i5 i6) (- zpp))
              (<= i7 (- rq)) ;; returns quality threshold
)])
         :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps 500
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 60
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           i1 (indicators/returns-from-open indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                           i10 (indicators/returns-quality indicator-state tic date)
                           dispersion @(:dispersion @((indicator-state tic) date))
                           price @(:price @((indicator-state tic) date))
                           time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                       {:header
                        ["date" "time" "tic" "price" (:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i9) (:header i10)]

                        :data
                        [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i9) (:data i10)]})}))

(defn agent-5-k-filter
  "complex strat that enters here exits there, with parameters
dispersion limited and not moving
high zpp enter short
returns quality poor
pnl bps target small
"

  [zpp rq pnl-target-bps]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3
                i8 (:data (indicators/moving-dispersion indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))
                i5 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i6 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                i7 (:data (indicators/returns-quality indicator-state tic date))]
            [(and
              (< dispersion 200)
              (zero? i8)
              (<= (- i5 i6) (- zpp))
              (<= i7 rq)  ;; returns quality threshold
)
             (and
              (< dispersion 200)
              (zero? i8)
              (>= (- i5 i6) zpp)
              (<= i7 rq)  ;; returns quality threshold
)])
         :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps 500
         :n-trades (atom 0.0)
         :max-n-trades 10.0 ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 60
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           i1 (indicators/returns-from-open indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                           i10 (indicators/returns-quality indicator-state tic date)
                           dispersion @(:dispersion @((indicator-state tic) date))
                           price @(:price @((indicator-state tic) date))
                           time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                       {:header
                        ["date" "time" "tic" "price" (:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i9) (:header i10)]

                        :data
                        [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i9) (:data i10)]})}))

(defn agent-6-rare-trader
  "complex strat that enters here exits there, with parameters
zpp
pnl bps target
"

  [zpp dd-bps]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3
                i8 (:data (indicators/moving-dispersion indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))
                i5 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i6 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                i7 (:data (indicators/returns-quality indicator-state tic date))]
            [(and
              (> dispersion 300)
              (not (zero? i8))
              (>= (- i5 i6) zpp)
              (>= i7 0.30)  ;; returns quality threshold
)
             (and
              (> dispersion 300)
              (not (zero? i8))
              (<= (- i5 i6) (- zpp))
              (<= i7 (- 0.30)) ;; returns quality threshold
)])
         :pnl-target-bps nil
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps dd-bps
         :n-trades (atom 0.0)
         :max-n-trades 1.0 ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 300
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           i1 (indicators/returns-from-open indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                           i10 (indicators/returns-quality indicator-state tic date)
                           dispersion @(:dispersion @((indicator-state tic) date))
                           price @(:price @((indicator-state tic) date))
                           time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                       {:header
                        ["date" "time" "tic" "price" (:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i9) (:header i10)]

                        :data
                        [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i9) (:data i10)]})}))

(defn agent-7-returns-quality-mr
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [zpp pnl-target-bps r-o max-n-trades agent-name]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3

                i9 (incanter/abs (:data (indicators/returns-from-open indicator-state tic date)))

                i7 (- (/ (:data (indicators/returns-quality indicator-state tic date)) 0.15))]
            [(and
              (>= i9 r-o)
              (>= i7 zpp) ;; returns quality threshold
)

             (and
              (>= i9 r-o)
              (<= i7 (- zpp)) ;; returns quality threshold
)])
         :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps 500
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name agent-name
         :max-ttc-minutes 60
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           i1 (indicators/returns-from-open indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                           i10 (indicators/returns-quality indicator-state tic date)
                           i11 (- (/ (:data (indicators/returns-quality indicator-state tic date)) 0.15))

                           long-signal   (cond (and
                                                (>= (incanter/abs (:data i1)) r-o)
                                                (>= i11 zpp) ;; returns quality threshold
                                                )1 :else 0)

                           short-signal (cond (and
                                               (>= (incanter/abs (:data i9)) r-o)
                                               (<= i11 (- zpp)) ;; returns quality threshold
                                               )1 :else 0) dispersion @(:dispersion @((indicator-state tic) date))
                           price @(:price @((indicator-state tic) date))
                           time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                       {:header
                        ["date" "time" "tic" "price" (:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i9) (:header i10) "i11" "long-signal" "short-signal"]

                        :data
                        [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i9) (:data i10) i11 long-signal short-signal]})}))

(defn agent-7-returns-quality-trend
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [zpp pnl-target-bps r-o max-n-trades agent-name]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3

                i9 (incanter/abs (:data (indicators/returns-from-open indicator-state tic date)))

                i7 (/ (:data (indicators/returns-quality indicator-state tic date)) 0.15)]
            [(and
              (>= i9 r-o)
              (>= i7 zpp) ;; returns quality threshold
)

             (and
              (>= i9 r-o)
              (<= i7 (- zpp)) ;; returns quality threshold
)])
         :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps 500
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name agent-name
         :max-ttc-minutes 60
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           i1 (indicators/returns-from-open indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                           i10 (indicators/returns-quality indicator-state tic date)
                           i11 (- (/ (:data (indicators/returns-quality indicator-state tic date)) 0.15))

                           long-signal   (cond (and
                                                (>= (incanter/abs (:data i1)) r-o)
                                                (>= i11 zpp) ;; returns quality threshold
                                                )1 :else 0)

                           short-signal (cond (and
                                               (>= (incanter/abs (:data i9)) r-o)
                                               (<= i11 (- zpp)) ;; returns quality threshold
                                               )1 :else 0) dispersion @(:dispersion @((indicator-state tic) date))
                           price @(:price @((indicator-state tic) date))
                           time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                       {:header
                        ["date" "time" "tic" "price" (:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i9) (:header i10) "i11" "long-signal" "short-signal"]

                        :data
                        [(name date) time (name tic) price (:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i9) (:data i10) i11 long-signal short-signal]})}))

(defn agent-8-lm
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [zpp pnl-target-bps max-n-trades]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3

                i1 (incanter/abs (:data (indicators/returns-from-open indicator-state tic date)))
                i2 (:data (indicators/recent-returns indicator-state tic date))
                i3 (:data (indicators/stdev-ema indicator-state tic date))
                i4 (:data (indicators/volume-ema indicator-state tic date))
                i5 (:data (indicators/price-ema 5 indicator-state tic date))
                i6 (:data (indicators/price-ema 12 indicator-state tic date))
                i7 (:data (indicators/price-ema 26 indicator-state tic date))
                i8 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i9 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                bidask (:data (indicators/bidask indicator-state tic date))

                emadiff (- i8 i9)
                i10 (:data (indicators/returns-quality indicator-state tic date))
                signal (reduce + (map * [i10 bidask emadiff] [-0.0001 8.69 -0.002]))
;returns-from-open              bidask             emadiff
;-8e-06        0.45       -1e-04
]
            [(and
              (>= i1 650)
              (>= signal zpp))

             (and
              (>= i1 650)
              (<= signal (- zpp)))])
         :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps 500
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 60
         :datagen nil}))

(defn header-fn [x] (:header x))
(defn data-fn [x] (:data x))

(defn agent-9-datagen

  []

  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3]
            [false false])
         :pnl-target-bps 100
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)

         :max-draw-bps 500
         :n-trades (atom 0.0)
         :max-n-trades 1 ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name "datagen"
         :max-ttc-minutes 60
         :max-pnl-this-round (atom (- 99.0))
         :min-pnl-this-round (atom 99.0)
         :allowed-to-enter? (atom true)
         :datagen   #(let [indicator-state %1
                           tic %2
                           date %3
                           mkt :SPY

                           i1 (indicators/returns-from-open-smooth indicator-state tic date)
                           i2 (indicators/recent-returns indicator-state tic date)
                           i3 (indicators/stdev-ema 5 indicator-state tic date)
                           i4 (indicators/volume-ema indicator-state tic date)
                           i5 (indicators/price-ema 5 indicator-state tic date)
                           i6 (indicators/price-ema 12 indicator-state tic date)
                           i7 (indicators/price-ema 26 indicator-state tic date)
                           i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                           i11 (indicators/moving-dispersion indicator-state tic date)
                           i12-10 (indicators/linear-regression-slope 10 indicator-state tic date)
                           i12-5 (indicators/linear-regression-slope 5 indicator-state tic date)
                           i12-15 (indicators/linear-regression-slope 15 indicator-state tic date)
                           i12-10-spy (indicators/linear-regression-slope 10 indicator-state mkt date)
                           i12-5-spy (indicators/linear-regression-slope 5 indicator-state mkt date)
                           i12-15-spy (indicators/linear-regression-slope 15 indicator-state mkt date)
                           r2 (indicators/r-square indicator-state tic date)
                           r2-spy (indicators/r-square indicator-state mkt date)
                           i13 (indicators/linear-regression-slope-diff indicator-state tic date)
                           i13-spy (indicators/linear-regression-slope-diff indicator-state mkt date)
                           last-return (indicators/last-return 10 indicator-state tic date)
                           bidask (indicators/bidask indicator-state tic date)
                           price @(:price @((indicator-state tic) date))
                           emadiff1 (- price (:data i5))
                           emadiff3 (cond (Double/isNaN (:data i7)) emadiff1 :else (- (:data i6) (:data i7)))
                           emadiff2 (cond (Double/isNaN (:data i7)) emadiff1 :else (- (:data i5) (:data i6)))
                           dispersion @(:dispersion @((indicator-state tic) date))
                           bv @(:buyvolume @((indicator-state tic) date))
                           sv @(:sellvolume @((indicator-state tic) date))
                           bidprice @(:bidprice @((indicator-state tic) date))
                           askprice @(:askprice @((indicator-state tic) date))
                           seq @(:seq-no @((indicator-state tic) date))
                           i16 (indicators/cumulative-relative-volume indicator-state tic date)]
                       (let [ilist [i1
                                    i2
                                    i3
                                    i4
                                    i5
                                    i6
                                    i7
                                    i8
                                    i11
                                    i12-10
                                    i12-5
                                    i12-15
                                    i12-10-spy
                                    i12-5-spy
                                    i12-15-spy
                                    r2
                                    r2-spy
                                    i13
                                    i13-spy
                                    last-return
                                    bidask
                                    emadiff1
                                    emadiff3
                                    emadiff2
                                    dispersion
                                    bv
                                    sv
                                    seq
                                    i16]]
                         {:header
                          ["date" "seq" "price" "bidprice" "askprice" (map header-fn ilist)]
                          :data
                          [(name date) seq price bidprice askprice (map data-fn ilist)]}))}))

(defn agent-10-market-make
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [price-band-cents pnl-target-bps max-n-trades]
  (atom {:commit
         #(let [indicator-state %1
                tic %2
                date %3

                i1 (incanter/abs (:data (indicators/returns-from-open indicator-state tic date)))
                i2 (:data (indicators/recent-returns indicator-state tic date))
                i3 (:data (indicators/stdev-ema indicator-state tic date))
                i4 (:data (indicators/volume-ema indicator-state tic date))
                i5 (:data (indicators/price-ema 5 indicator-state tic date))
                i6 (:data (indicators/price-ema 12 indicator-state tic date))
                i7 (:data (indicators/price-ema 26 indicator-state tic date))
                i8 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i9 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                bidask (:data (indicators/bidask indicator-state tic date))

                emadiff (- i8 i9)
                i10 (:data (indicators/returns-quality indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))

;returns-from-open              bidask             emadiff
;-8e-06        0.45       -1e-04
]
            (< i3 10))
         :commit-lock (atom false) ;; if this is true stop update price band
         :price-band-upper (atom 1000.0) ;; commit true, then lock commit-lock and do not update band
         :price-band-lower (atom -1000.0)
         :price-band-cents price-band-cents
         :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps 500
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 60
         :datagen nil}))

(defn agent-11-market-make
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [commit-vol price-band-cents pnl-target-bps dd max-n-trades agent-name]
  (atom {:commit
         #(let [indicator-state %1
                tic %2
                date %3

                i1 (incanter/abs (:data (indicators/returns-from-open indicator-state tic date)))
                i2 (:data (indicators/recent-returns indicator-state tic date))
                i3 (:data (indicators/stdev-ema 5 indicator-state tic date))
                i4 (:data (indicators/volume-ema indicator-state tic date))
                i5 (:data (indicators/price-ema 5 indicator-state tic date))
                i6 (:data (indicators/price-ema 12 indicator-state tic date))
                i7 (:data (indicators/price-ema 26 indicator-state tic date))
                i8 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i9 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                bidask (:data (indicators/bidask indicator-state tic date))

                emadiff (- i8 i9)
                 ;i10 (:data (indicators/returns-quality indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))

;returns-from-open              bidask             emadiff
;-8e-06        0.45       -1e-04
]
            [(>= i3 commit-vol)  (pos? (- i5 i6))  (neg? (- i5 i6))]) :commit-lock (atom false) ;; if this is true stop update price band
         :price-band-upper (atom 1000.0) ;; commit true, then lock commit-lock and do not update band
         :price-band-lower (atom -1000.0)
         :price-band-cents price-band-cents
         :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps dd
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :agent-name agent-name
         :max-ttc-minutes 600
         :print-datagen? false
         :datagen nil
         :live? (atom true)
         :allowed-to-enter? (atom true)
         :max-pnl (atom 0.0)
         :min-pnl (atom 0.0)
         :max-position (atom 0.0)
         :min-position (atom 0.0)
         :eod-position (atom 0.0)

         :cash-account (atom 0.0) :m2m-pnl (atom 0.0)
         :jutsu false
         :jutsu2 #(let [indicator-state %1
                        tic %2
                        date %3
                        price @(:price @((indicator-state tic) date))
                        time @(:seq-no @((indicator-state tic) date))
                ;i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                        i12 (:data (indicators/linear-regression-slope 15 15 indicator-state tic date))
                        i14 (:data (indicators/returns-from-open indicator-state tic date))
                        i15 (:data (indicators/trade-time-lapse indicator-state tic date))
                        i16 (:data (indicators/cumulative-relative-volume indicator-state tic date))]

                                        ;[date tic price i1 i2 ]

                    [{:plotly-data   {:data {:y [[i14]] :x [[time]]}
                                      :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                      :chart (str (tickers/ticker->sector (name tic)) "-prices")}

                     {:plotly-data   {:data {:y [[i16]] :x [[time]]}
                                      :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                      :chart (str (tickers/ticker->sector (name tic)) "-volumes")}])}))

(defn agent-11-cross-sectional
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [entry-threshold exit-threshold pnl-target-bps dd max-n-trades agent-name live?]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3

                i1 (incanter/abs (:data (indicators/returns-from-open indicator-state tic date)))
                i2 (:data (indicators/recent-returns indicator-state tic date))
                i3 (:data (indicators/stdev-ema 5 indicator-state tic date))
                i4 (:data (indicators/volume-ema indicator-state tic date))
                i5 (:data (indicators/price-ema 5 indicator-state tic date))
                i6 (:data (indicators/price-ema 12 indicator-state tic date))
                i7 (:data (indicators/price-ema 26 indicator-state tic date))
                i8 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i9 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                i10 (:data (indicators/returns-from-open-zscore indicator-state tic tickers/portfolio date))
                bidask (:data (indicators/bidask indicator-state tic date))
          ;dummy (pprint i10)

                emadiff (- i8 i9)
                 ;i10 (:data (indicators/returns-quality indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))

;returns-from-open              bidask             emadiff
;-8e-06        0.45       -1e-04
]
            [(<= entry-threshold i10 exit-threshold) (<= (- exit-threshold) i10   (- entry-threshold)) (> i10 exit-threshold) (< i10 (- exit-threshold))]) :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps dd
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :agent-name agent-name
         :max-ttc-minutes 600
         :print-datagen? false
         :datagen nil
         :live? (atom live?)
         :allowed-to-enter? (atom true)
         :max-pnl (atom 0.0)
         :min-pnl (atom 0.0)
         :max-position (atom 0.0)
         :min-position (atom 0.0)
         :eod-position (atom 0.0)

         :cash-account (atom 0.0) :m2m-pnl (atom 0.0)
         :jutsu false
         :jutsu2 #(let [indicator-state %1
                        tic %2
                        date %3
                        price @(:price @((indicator-state tic) date))
                        time @(:seq-no @((indicator-state tic) date))
                ;i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                        i12 (:data (indicators/linear-regression-slope 15 15 indicator-state tic date))
                        i14 (:data (indicators/returns-from-open indicator-state tic date))
                        i15 (:data (indicators/trade-time-lapse indicator-state tic date))
                        i16 (:data (indicators/cumulative-relative-volume indicator-state tic date))]

                                        ;[date tic price i1 i2 ]

                    [{:plotly-data   {:data {:y [[i14]] :x [[time]]}
                                      :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                      :chart (str (tickers/ticker->sector (name tic)) "-prices")}

                     {:plotly-data   {:data {:y [[i16]] :x [[time]]}
                                      :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                      :chart (str (tickers/ticker->sector (name tic)) "-volumes")}])}))

(defn agent-11-position-target
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [entry-threshold exit-threshold pnl-target-bps dd max-n-trades agent-name live?]
  (atom {:target-position
         #(let [indicator-state %1
                tic %2
                date %3
                mkt :SPY

                i1 (indicators/returns-from-open-smooth indicator-state tic date)
                i2 (indicators/recent-returns indicator-state tic date)
                i3 (indicators/stdev-ema 5 indicator-state tic date)
                i4 (indicators/volume-ema indicator-state tic date)
                i5 (indicators/price-ema 5 indicator-state tic date)
                i6 (indicators/price-ema 12 indicator-state tic date)
                i7 (indicators/price-ema 26 indicator-state tic date)
                i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                i11 (indicators/moving-dispersion indicator-state tic date)
                i12-10 (indicators/linear-regression-slope 10 indicator-state tic date)
                i12-5 (indicators/linear-regression-slope 5 indicator-state tic date)
                i12-15 (indicators/linear-regression-slope 15 indicator-state tic date)
                i12-10-spy (indicators/linear-regression-slope 10 indicator-state mkt date)
                i12-5-spy (indicators/linear-regression-slope 5 indicator-state mkt date)
                i12-15-spy (indicators/linear-regression-slope 15 indicator-state mkt date)
                r2 (indicators/r-square indicator-state tic date)
                r2-spy (indicators/r-square indicator-state mkt date)
                i13 (indicators/linear-regression-slope-diff indicator-state tic date)
                i13-spy (indicators/linear-regression-slope-diff indicator-state mkt date)
                last-return (indicators/last-return 10 indicator-state tic date)
                bidask (indicators/bidask indicator-state tic date)
                p @(:price @((indicator-state tic) date))
                emadiff1 (- p (:data i5))
                emadiff3 (cond (Double/isNaN (:data i7)) emadiff1 :else (- (:data i6) (:data i7)))
                emadiff2 (cond (Double/isNaN (:data i7)) emadiff1 :else (- (:data i5) (:data i6)))
                dispersion @(:dispersion @((indicator-state tic) date))
                bv @(:buyvolume @((indicator-state tic) date))
                sv @(:sellvolume @((indicator-state tic) date))
                seq @(:seq-no @((indicator-state tic) date))
                i16 (indicators/cumulative-relative-volume indicator-state tic date)]
            (cond (and
             ;(>= i12 1.0)
             ;(pos? i13)
             ;(pos? i1)
             ;(>= i16 1 )
                   (>= (:data i13) 0.50)
             ;(pos? emadiff1)
             ;(pos? emadiff2)
                                        ;(pos? emadiff3)

             ;(<= last-return-zscore (- 3.0))
             ;(>=  (:data autocorr) 0.15 )
             ;(pos? (:data last-return))
                                        ;(> seq 200)
                   (>= (:data r2) 0.98)
             ;(identity false)
             ;(not (zero? i11))
                   (> seq 30)
             ;(> sharpe 0.09 )
             ;(>= variance 0.20)
                 ;(not (zero? i11))
                                        ;(< entry-threshold i10 exit-threshold )
;(pos? mkt-trend1) (pos? mkt-trend2)
)
                  [1 [seq (:data r2)]]
            ;(>= i10 exit-threshold) 0.0
                  (and
             ;(>= i16 1 )
                 ;(<= i12 (- 1.0))
             ;(neg? i1)
             ;(<= i12 (- 0.50))
             ;(neg? emadiff1)
             ;(>= r2 0.70)
                                        ;(not (zero? i11))
             ;(>= last-return-zscore 3.0)
             ;(>=  (:data autocorr) 0.15 )
             ;(neg? (:data last-return))
                   (>= (:data r2) 0.98)
                   (<= (:data i13) (- 0.50))
             ;(identity false)
             ;(not (zero? i11))
                   (> seq 30)

             ;(neg? emadiff1) (neg? emadiff2) (neg? emadiff3)
             ;(<= signal (- 2.0))
             ;(> seq 20)
             ;(< sharpe (- 0.09) )
             ;(>= variance 0.20)

                 ;(not (zero? i11))
                 ;(neg? mkt-trend1) (neg? mkt-trend2)
                 ;(< (- exit-threshold) i10 (- entry-threshold) )
)
                  [(- 1) [seq (:data r2)]]
            ;(<= i10 (- exit-threshold)) 0.0
                  :else [Double/NaN Double/NaN]))

         :exit-position  ;; if this is short exit long
         #(let [indicator-state %1
                tic %2
                date %3
                i1 (:data (indicators/returns-from-open indicator-state tic date))
                i2 (:data (indicators/recent-returns indicator-state tic date))
                i3 (:data (indicators/stdev-ema 5 indicator-state tic date))
                i4 (:data (indicators/volume-ema indicator-state tic date))
                i5 (:data (indicators/price-ema 5 indicator-state tic date))
                i6 (:data (indicators/price-ema 12 indicator-state tic date))
                i7 (:data (indicators/price-ema 26 indicator-state tic date))
                i8 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i9 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                i11 (:data (indicators/moving-dispersion indicator-state tic date))
                bidask (:data (indicators/bidask indicator-state tic date))
                p @(:price @((indicator-state tic) date))
                emadiff1 (- p i5)
                emadiff3 (cond (Double/isNaN i7) emadiff1 :else (- i6 i7))
                emadiff2 (cond (Double/isNaN i7) emadiff1 :else (- i5 i6))
          ;i14 (:data (indicators/price-ema 5 indicator-state :SPY date))
          ;i15 (:data (indicators/price-ema 12 indicator-state :SPY date))
          ;mkt-price (:data (indicators/l1-price indicator-state :SPY date))
          ;mkt-trend1 (- mkt-price i14)
          ;mkt-trend2 (- i14 i15)
]
            (cond (and
                   (pos? i1)
                   (pos? emadiff2)
             ;(pos? mkt-trend2)
)   ;(< entry-threshold i10 exit-threshold )
                  1.0
                  (and
                   (neg? i1)
                   (neg? emadiff2)
                  ;(neg? mkt-trend2)
)
                  (- 1.0)
                  :else 0.0)) :reference-price
         #(let [indicator-state %1
                tic %2
                date %3
                i5 (:data (indicators/price-ema 5 indicator-state tic date))]
            i5)

         :max-otl2
         #(let [indicator-state %1
                tic %2
                date %3
                i5 (:data (indicators/bidask indicator-state tic date))
                i6 (:data (indicators/stdev-price 5 indicator-state tic date))
                i7 (:data (indicators/l1-price indicator-state tic date))
                calc-otl (* 10000 (/ (+ i5 (* 2 i6)) i7))
          ;dummy (pprint (str tic " otl " calc-otl ))
]
            calc-otl)

         :target-size  ;; if otl is high take a smaller size
         #(let [indicator-state %1
                tic %2
                date %3
                i5 (:data (indicators/bidask indicator-state tic date))
                i6 (:data (indicators/stdev-price 20 indicator-state tic date))
                i7 (:data (indicators/l1-price indicator-state tic date))
                size-to-take-nan (int (/ 100 (+ i5 (* 2 i6))))
                size-to-take (cond (Double/isNaN size-to-take-nan) Double/NaN :else (apply max [100 size-to-take-nan]))
          ;dummy (pprint (str tic " otl " calc-otl ))
]
            size-to-take) :pnl-target-bps pnl-target-bps
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :position-status (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps dd
         :n-trades (atom 0.0)
         :n-otls (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-otls 1.0
         :max-loss-per-tic nil ;; need to think through this
         :agent-name agent-name
         :max-ttc-minutes 5.0
         :print-datagen? false
         :datagen nil :live? (atom live?)
         :allowed-to-enter-short? (atom true)
         :allowed-to-enter-long? (atom true)
         :max-pnl (atom (- 99.0))
         :min-pnl (atom 99.0)
         :max-position (atom 0.0)
         :min-position (atom 0.0)
         :eod-position (atom 0.0)

         :cash-account (atom 0.0) :m2m-pnl (atom 0.0)
         :jutsu false
         :max-otl (atom 100.0)
         :break-after-otl-minutes 0.50
         :otl-start-time (atom Double/NaN) ;; time when otl is started
         :max-total-pnl (atom -99.0)
         :max-spread 0.10}))

(defn agent-12-lowstdev-trend
  [stdev-limit lm-entry max-n-trades dd]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3 i3 (:data (indicators/stdev-ema indicator-state tic date))
                i11 (:data (indicators/linear-regression-slope 15 15 indicator-state tic date))
                i12 (:data (indicators/linear-regression-slope-diff 15 15 indicator-state tic date))]
            [(and
              (< i3 stdev-limit)
              (> i11 lm-entry) ;; returns quality threshold
              (> i12 0.0) ;; returns quality threshold
)

             (and
              (< i3 stdev-limit)
              (< i11 (- lm-entry))  ;; returns quality threshold
              (< i12 0.0) ;; returns quality threshold
)])
         :exit
         #(let [indicator-state %1
                tic %2
                date %3 i3 (:data (indicators/stdev-ema indicator-state tic date))

                i11 (:data (indicators/linear-regression-slope 15 15 indicator-state tic date))
                i12 (:data (indicators/linear-regression-slope-diff 15 15 indicator-state tic date))]
            [(or
      ;(>= i3 stdev-limit )
              (< i11 0.0))

             (or
      ;(>= i3 stdev-limit )
              (> i11 0.0))])

         :pnl-target-bps nil
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom 0.0)
         :max-draw-bps dd ;; some sanity number not expect to trigger
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 120 ;; again some sane number
         :datagen #(let [indicator-state %1
                         tic %2
                         date %3

                         i3 (indicators/stdev-ema indicator-state tic date)
                         i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                         i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)

                         price @(:price @((indicator-state tic) date))
                         time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                     {:header
                      ["date" "time" "tic" "price" (:header i3) (:header i11) (:header i12)]

                      :data
                      [(name date) time (name tic) price (:data i3)  (:data i11) (:data i12)]})
         :print-datagen false}))

(defn agent-12-lowstdev-mr
  [lm-entry max-n-trades dd pnl-target smoothing lookback]
  (atom {:entry
         #(let [indicator-state %1
                tic %2
                date %3

                i1 (:data (indicators/returns-from-open indicator-state tic date))

                i3 (:data (indicators/stdev-ema indicator-state tic date))
                i11 (:data (indicators/linear-regression-slope smoothing lookback indicator-state tic date))
                i12 (:data (indicators/linear-regression-slope-diff smoothing lookback indicator-state tic date))]
            [(and
              (< (incanter/abs i1) 300) ;; only if moves are limited
;      (> i11 lm-entry) ;; returns quality threshold
              (< i11 (- lm-entry))
      ;(> i12 0.0) ;; returns quality threshold
)
             (and
              (< (incanter/abs i1) 300)
      ;(< i3 stdev-limit )
              (> i11 lm-entry)
      ;(< i12 0.0) ;; returns quality threshold
)])
         :exit
         #(let [indicator-state %1
                tic %2
                date %3

                i1 (:data (indicators/returns-from-open indicator-state tic date))
                i3 (:data (indicators/stdev-ema indicator-state tic date))

                i11 (:data (indicators/linear-regression-slope 15 15 indicator-state tic date))
                i12 (:data (indicators/linear-regression-slope-diff 15 15 indicator-state tic date))]
            [(> (incanter/abs i1) 300)
             (> (incanter/abs i1) 300)])

         :pnl-target-bps pnl-target
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom (- 99.0))
         :min-pnl-this-round (atom 99.0)

         :max-draw-bps dd ;; some sanity number not expect to trigger
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 120 ;; again some sane number
         :datagen #(let [indicator-state %1
                         tic %2
                         date %3

                         i3 (indicators/stdev-ema indicator-state tic date)
                         i11 (indicators/linear-regression-slope smoothing lookback indicator-state tic date)
                         i12 (indicators/linear-regression-slope-diff smoothing lookback indicator-state tic date)

                         price @(:price @((indicator-state tic) date))
                         time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                     {:header
                      ["date" "time" "tic" "price" (:header i3) (:header i11) (:header i12)]

                      :data
                      [(name date) time (name tic) price (:data i3)  (:data i11) (:data i12)]})
         :print-datagen true}))

(comment (defn agent-14-learned-tree
           " first tree model learnt from data"
           [max-n-trades dd pnl-target]

           (atom {;        :entry

                  :exit nil :entry   #(let [indicator-state %1
                                            tic %2
                                            date %3
                                            i1 (indicators/returns-from-open indicator-state tic date)
                                            i2 (indicators/recent-returns indicator-state tic date)
                                            i3 (indicators/stdev-ema indicator-state tic date)
                                            i4 (indicators/volume-ema indicator-state tic date)
                                            i5 (indicators/price-ema 5 indicator-state tic date)
                                            i6 (indicators/price-ema 12 indicator-state tic date)
                                            i7 (indicators/price-ema 26 indicator-state tic date)
                                            i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                                            i81 (indicators/pseudo-price-ema 12 indicator-state tic date)
                                            i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                                            i10 (indicators/returns-quality indicator-state tic date)
                                            i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                                            i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)
                                            dispersion @(:dispersion @((indicator-state tic) date))
                                            price @(:price @((indicator-state tic) date))
                                            time @(:seq-no @((indicator-state tic) date))
                                            indicator-output (zipmap

                                                              [(:header i1) (:header i2) (:header i3) (:header i4) "dispersion" (:header i5) (:header i6) (:header i7) (:header i8) (:header i81) (:header i9) (:header i10) (:header i11) (:header i12)] [(:data i1) (:data i2) (:data i3) (:data i4) dispersion (:data i5) (:data i6) (:data i7) (:data i8) (:data i81) (:data i9) (:data i10) (:data i11) (:data i12)])
                                            report-signal? (every? utils/notnan? (map second indicator-output))
                                        ;test (pprint indicator-output)
                                            signal (cond (not report-signal?) "0" :else (classify models/iris-tree-model1 indicator-output))
                                            test (pprint signal)]             ;[date tic price i1 i2 ]


                                        [(= signal "1") (= signal "-1")]) :pnl-target-bps pnl-target
                  :uts 100
                  :last-entry-price (atom [])
                  :last-exit-price (atom [])
                  :last-entry-time (atom 0.0) ;;msecs
                  :pnl (atom 0.0)
                  :position (atom 0.0)
                  :mur 100.0
                  :total-pnl (atom 0.0)
                  :m2m-pnl-bps (atom 0.0)
                  :max-pnl-this-round (atom (- 99.0))
                  :min-pnl-this-round (atom 99.0)

                  :max-draw-bps dd ;; some sanity number not expect to trigger
                  :n-trades (atom 0.0)
                  :max-n-trades max-n-trades ;; limit number of trades per tic
                  :max-loss-per-tic nil      ;; need to think through this
                  :name (str (gensym (utils/random-word)))
                  :max-ttc-minutes 120 ;; again some sane number
                  :allowed-to-enter? (atom true)
                  :datagen #(let [indicator-state %1
                                  tic %2
                                  date %3

                                  i3 (indicators/stdev-ema indicator-state tic date)
                                  i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                                  i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)

                                  price @(:price @((indicator-state tic) date))
                                  time @(:seq-no @((indicator-state tic) date))]             ;[date tic price i1 i2 ]
                              {:header
                               ["date" "time" "tic" "price" (:header i3) (:header i11) (:header i12)]

                               :data
                               [(name date) time (name tic) price (:data i3)  (:data i11) (:data i12)]})
                  :print-datagen false})))

(defn agent-15-simple-momentum
  " follow trend"
  [sharpe r-o max-n-trades dd rq-duration]
  (atom {:exit nil
         :entry   #(let [indicator-state %1
                         tic %2
                         date %3
                         i1 (indicators/returns-from-open indicator-state tic date)
                         i2 (indicators/recent-returns indicator-state tic date)
                         i3 (indicators/stdev-ema indicator-state tic date)
                         i4 (indicators/volume-ema indicator-state tic date)
                         i5 (indicators/price-ema 5 indicator-state tic date)
                         i6 (indicators/price-ema 12 indicator-state tic date)
                         i7 (indicators/price-ema 26 indicator-state tic date)
                         i8 (indicators/pseudo-price-ema 5 indicator-state tic date)
                         i81 (indicators/pseudo-price-ema 12 indicator-state tic date)
                         i9 (indicators/pseudo-price-ema 26 indicator-state tic date)
                         i10 (indicators/returns-quality rq-duration indicator-state tic date)
                         i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                         i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)
                         dispersion @(:dispersion @((indicator-state tic) date))
                         price @(:price @((indicator-state tic) date))
                         time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                     [(and
                       (>= (incanter/abs (:data i1)) r-o)
                       (>= (:data i10) sharpe) ;; returns quality threshold
                       (pos? (- (:data i5) (:data i6)))
      ;; add moving average hierarchy otherwise enters when signal is very volatile
)
                      (and
                       (>= (incanter/abs (:data i1)) r-o)
                       (<= (:data i10) (- sharpe)) ;; returns quality threshold
                       (neg? (- (:data i5) (:data i6))))])
         :pnl-target-bps nil
         :uts 100
         :last-entry-price (atom [])
         :last-exit-price (atom [])
         :last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :m2m-pnl-bps (atom 0.0)
         :max-pnl-this-round (atom (- 99.0))
         :min-pnl-this-round (atom 99.0)

         :max-draw-bps dd ;; some sanity number not expect to trigger
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 390 ;; again some sane number
         :allowed-to-enter? (atom true)
         :live? (atom true) ;; have live will place orders
         ;:jutsu nil
         :jutsu #(let [indicator-state %1
                       tic %2
                       date %3
                       price @(:price @((indicator-state tic) date))
                       time @(:seq-no @((indicator-state tic) date))
                ;i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                ;i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)
                       i14 (indicators/returns-from-open indicator-state tic date)]                       ;[date tic price i1 i2 ]
                   [;; first is the starter for plot, second is the update map to be sent to jutsu
                    [{:x [time]
                      :y [(:data i14)]
                      :mode "markers"
                      :type "scatter"}
                                        ;{:x [time] :y [(:data i11)] :mode "markers" :type "scatter"}
]
                    [{:data {:y [[(:data i14)]] :x [[time]]}
                      :traces [(@jutsudata/trace-id-map tic)]}
                                        ;{:data {:y [[(:data i11)]] :x [[time]]} :traces [1]}
                     ]]):datagen #(let [indicator-state %1
                                        tic %2
                                        date %3

                                        i3 (indicators/stdev-ema indicator-state tic date)
                                        i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                                        i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)

                                        price @(:price @((indicator-state tic) date))
                                        time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                                    {:header
                                     ["date" "time" "tic" "price" (:header i3) (:header i11) (:header i12)]

                                     :data
                                     [(name date) time (name tic) price (:data i3)  (:data i11) (:data i12)]})
         :print-datagen false}))

;(pprint (@jutsudata/trace-id-map :AAPL))

(defn agent-16-fernholz
  " fernholz mean reversion"
  [when-to-peg inventory-threshold max-n-trades agent-name]
  (atom {:target-position   #(let [indicator-state %1
                                   tic %2
                                   date %3

                                   i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                                   i14 (indicators/fernholz-target when-to-peg indicator-state tic date)

                                   dispersion @(:dispersion @((indicator-state tic) date))
                                   price @(:price @((indicator-state tic) date))
                                   time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                               (:data i14))
         :inventory-threshold inventory-threshold
         :pnl-target-bps nil
         ;:uts 100
         ;:last-entry-price (atom [])
         ;:last-exit-price (atom [])
         ;:last-entry-time (atom 0.0) ;;msecs
         :pnl (atom 0.0)
         :position (atom 0.0)
         ;:mur 100.0
         :total-pnl (atom 0.0)
         :max-pnl (atom 0.0)
         :min-pnl (atom 0.0)
         :max-position (atom 0.0)
         :min-position (atom 0.0)
         :eod-position (atom 0.0)

         :cash-account (atom 0.0)

         :m2m-pnl-bps (atom 0.0)
         :m2m-pnl (atom 0.0)

         ;:max-pnl-this-round (atom (- 99.0))
         ;:min-pnl-this-round (atom 99.0)

         :max-draw-bps 1000 ;; some sanity number not expect to trigger
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic
         :max-loss-per-tic nil ;; need to think through this
         :name (str (gensym (utils/random-word)))
         :max-ttc-minutes 390 ;; again some sane number
         :allowed-to-enter? (atom true)
         :live? (atom false) ;; have live will place orders
         :jutsu false
         :jutsu2 #(let [indicator-state %1
                        tic %2
                        date %3
                        price @(:price @((indicator-state tic) date))
                        time @(:seq-no @((indicator-state tic) date))
                        i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                ;i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)
                        i14 (:data (indicators/returns-from-open indicator-state tic date))
                        i15 (:data (indicators/trade-time-lapse indicator-state tic date))
                        i16 (:data (indicators/cumulative-relative-volume indicator-state tic date))]                       ;[date tic price i1 i2 ]
                    [;; first is the starter for plot, second is the update map to be sent to jutsu
                     [{:x [time]

                       :y [price]

                       :mode "markers"
                       :type "scatter"
                       :name (name tic)}
                                        ;{:x [time] :y [(:data i11)] :mode "markers" :type "scatter"}
]
                     [{:plotly-data   {:data {:y [[i14]] :x [[time]]}
                                       :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                       :chart (str (tickers/ticker->sector (name tic)) " prices")}

                      {:plotly-data   {:data {:y [[i16]] :x [[time]]}
                                       :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                       :chart (str (tickers/ticker->sector (name tic)) " volumes")}

                      {:plotly-data   {:data {:y [[price]] :x [[time]]}
                                       :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                       :chart (str (tickers/ticker->sector (name tic)) " realprices")}]]) :datagen #(let [indicator-state %1
                                                                                                          tic %2
                                                                                                          date %3

                                                                                                          i3 (indicators/stdev-ema indicator-state tic date)
                                                                                                          i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                                                                                                          i12 (indicators/linear-regression-slope-diff 15 15 indicator-state tic date)

                                                                                                          price @(:price @((indicator-state tic) date))
                                                                                                          time @(:seq-no @((indicator-state tic) date))]            ;[date tic price i1 i2 ]
                                                                                                      {:header
                                                                                                       ["date" "time" "tic" "price" (:header i3) (:header i11) (:header i12)]

                                                                                                       :data
                                                                                                       [(name date) time (name tic) price (:data i3)  (:data i11) (:data i12)]})
         :print-datagen false
         :agent-name agent-name}))

(defn agent-17-base
  "complex strat that enters here exits there, with parameters
zpp
returns quality
pnl bps target
"

  [max-n-trades agent-name]
  (atom {:signal
         #(let [indicator-state %1
                tic %2
                date %3

                i1 (incanter/abs (:data (indicators/returns-from-open indicator-state tic date)))
                i2 (:data (indicators/recent-returns indicator-state tic date))
                i3 (:data (indicators/stdev-ema indicator-state tic date))
                i4 (:data (indicators/volume-ema indicator-state tic date))
                i5 (:data (indicators/price-ema 5 indicator-state tic date))
                i6 (:data (indicators/price-ema 12 indicator-state tic date))
                i7 (:data (indicators/price-ema 26 indicator-state tic date))
                i8 (:data (indicators/pseudo-price-ema 5 indicator-state tic date))
                i9 (:data (indicators/pseudo-price-ema 26 indicator-state tic date))
                bidask (:data (indicators/bidask indicator-state tic date))
                lr (:data (indicators/last-return indicator-state tic date))

                emadiff (- i8 i9)
                 ;i10 (:data (indicators/returns-quality indicator-state tic date))
                dispersion @(:dispersion @((indicator-state tic) date))

;returns-from-open              bidask             emadiff
;-8e-06        0.45       -1e-04
]
            (cond (>= lr 0.001) (- 1.00)  (<= lr (- 0.001)) 1.00 :else 0.0))

         :uts 100
         :pnl (atom 0.0)
         :position (atom 0.0)
         :mur 100.0
         :total-pnl (atom 0.0)
         :n-trades (atom 0.0)
         :max-n-trades max-n-trades ;; limit number of trades per tic

         :agent-name agent-name

         :print-datagen? false
         :datagen nil
         :live? (atom false)
         :allowed-to-enter? (atom true)
         :max-pnl (atom 0.0)
         :min-pnl (atom 0.0)
         :max-position (atom 0.0)
         :min-position (atom 0.0)
         :eod-position (atom 0.0)

         :cash-account (atom 0.0) :m2m-pnl (atom 0.0)
         :jutsu #(let [indicator-state %1
                       tic %2
                       date %3
                       price @(:price @((indicator-state tic) date))
                       time @(:seq-no @((indicator-state tic) date))
                ;i11 (indicators/linear-regression-slope 15 15 indicator-state tic date)
                       i12 (:data (indicators/linear-regression-slope 15 15 indicator-state tic date))
                       i14 (:data (indicators/returns-from-open indicator-state tic date))
                       i15 (:data (indicators/trade-time-lapse indicator-state tic date))
                       i16 (:data (indicators/cumulative-relative-volume indicator-state tic date))]                       ;[date tic price i1 i2 ]
                   [;; first is the starter for plot, second is the update map to be sent to jutsu
                    [{:x [time]

                      :y [price]

                      :mode "markers"
                      :type "scatter"
                      :name (name tic)}
                                        ;{:x [time] :y [(:data i11)] :mode "markers" :type "scatter"}
]
                    [{:plotly-data   {:data {:y [[i14]] :x [[time]]}
                                      :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                      :chart (str (tickers/ticker->sector (name tic)) " prices")}

                     {:plotly-data   {:data {:y [[i16]] :x [[time]]}
                                      :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                      :chart (str (tickers/ticker->sector (name tic)) " volumes")}

                     {:plotly-data   {:data {:y [[i12]] :x [[time]]}
                                      :traces [(@(jutsudata/trace-id-map (keyword (tickers/ticker->sector (name tic)))) tic)]
               ;:chart "prices"
}
                      :chart (str (tickers/ticker->sector (name tic)) " LM")}]])}));zpp rq pnl-target-bps
;returns-from-open              bidask             emadiff
;-8e-06        0.45       -1e-04

(def a-list-datagen
  [;     #(agent-11-market-make 0.03 100 200 3)
   #(agent-9-datagen)])

(def a-list [;#(agent-5-k-filter 30 0.20 20 )
             ;#(agent-5-k-filter 20 0.20 20 )
             ;#(agent-5-k-filter 10 0.20 20 )
             ;#(agent-5-k-filter 1 0.20 20 )

    ;         #(agent-5-k-filter 40 0.20 10 )
     ;        #(agent-5-k-filter 50 0.20 30 )
;#(map agent-4-profit-taker [30 40 50] [0.10 0.20 0.30] [40 50 60] )
             ;#(agent-4-profit-taker 30 0.20 20 )
             ;#(agent-4-profit-taker 40 0.20 30 )
             ;#(agent-4-profit-taker 50 0.20 40 )

             ;#(agent-4-profit-taker 30 0.20 50 ) ;; seems like a sweet spot


             ;#(agent-4-profit-taker 20 0.20 50 1 ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.75 50 400 1 "1" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 1.00 20 400 1 "2" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.50 10 400 1 "3" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 1.15 10 400 1 "4" ) ;; seems like a sweet spot


            ; #(agent-7-returns-quality-mr 0.75 30 500 1 "5" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.75 30 500 2 "6" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.75 30 500 3 "7" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.75 30 500 4 "8" ) ;; seems like a sweet spot

            ; #(agent-7-returns-quality-mr 0.75 30 500 5 "9" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.75 30 500 6 "10" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.75 30 500 7 "11" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.75 30 500 8 "12" ) ;; seems like a sweet spot
; #(agent-7-returns-quality-trend 0.75 10 500 1 "5" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-trend 0.75 5 500 1 "6" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-trend 0.75 20 500 1 "7" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-trend 0.75 30 500 1 "8" ) ;; seems like a sweet spot

             ;#(agent-10-market-make 0.01 50 1 ) ;; seems like a sweet spot
             ;#(agent-10-market-make 0.03 50 1 ) ;; seems like a sweet spot
             ;#(agent-10-market-make 0.05 50 1 ) ;; seems like a sweet spot
             ;#(agent-10-market-make 0.10 50 1 ) ;; seems like a sweet spot

             ;#(agent-10-market-make 0.01 50 1 ) ;; seems like a sweet spot
             ;#(agent-10-market-make 0.03 50 3 ) ;; seems like a sweet spot
             ;#(agent-10-market-make 0.05 50 5 ) ;; seems like a sweet spot
             ;#(agent-10-market-make 0.10 50 10 ) ;; seems like a sweet spot

             ;#(agent-10-market-make 0.01 50 1 ) ;; seems like a sweet spot
             ;#(agent-11-market-make 20 0.02 20 200 20 "20" )
             ;#(agent-11-market-make 20 0.02 200 200 20 "200" )
             ;#(agent-11-market-make 5 0.02 400 400 20 "400" )
             ;#(agent-11-cross-sectional 2.0 2.5 400 400 20 "400" true )
             #(agent-11-position-target 2.0 3.0 400 400 20 "2.0-3.0-400-400-2" true) ;; 5 round trips should be enough
             ;#(agent-11-market-make 30 0.02 20 200 20 "30" )
             ;#(agent-11-market-make 40 0.02 20 200 20 "40" )
             ;#(agent-11-market-make 15 0.02 20 200 20 "15" )
             ;#(agent-17-base 10 "10" )

             ;#(agent-11-market-make 0.05 50 1 ) ;; seems like a sweet spot
             ;#(agent-11-market-make 0.10 50 1 ) ;; seems like a sweet spot

             ;#(agent-11-market-make 0.01 50 1 ) ;; seems like a sweet spot
             ;#(agent-11-market-make 0.03 50 3 ) ;; seems like a sweet spot
             ;#(agent-11-market-make 0.05 50 5 ) ;; seems like a sweet spot
             ;#(agent-11-market-make 0.10 50 10 ) ;; seems like a sweet spot

             ;[price-band-cents pnl-target-bps dd max-n-trades]
             ;#(agent-11-market-make 0.03 100 50 3)
             ;#(agent-11-market-make 0.03 200 50 3)
             ;#(agent-11-market-make 0.03 300 50 3)
             ;#(agent-11-market-make 0.03 400 50 3)

             ;#(agent-11-market-make 0.03 100 150 3)
             ;#(agent-11-market-make 0.03 200 150 3)
             ;#(agent-11-market-make 0.03 300 150 3)
             ;#(agent-11-market-make 0.03 400 150 3)

             ;#(agent-11-market-make 0.05 100 500 3)
             ;#(agent-11-market-make 0.05 200 500 3)
             ;#(agent-11-market-make 0.05 300 500 3)
             ;#(agent-11-market-make 0.05 400 500 3)
;   #(agent-12-lowstdev-mr 3.0 1 100 20)
         ;    #(agent-12-lowstdev-mr 5.0 1 100 20)
        ;     #(agent-12-lowstdev-mr 7.0 1 100 20)
       ;      #(agent-12-lowstdev-mr 9.0 1 100 20)

      ;       #(agent-12-lowstdev-mr 3.0 1 200 10)
     ;        #(agent-12-lowstdev-mr 5.0 1 200 10)
    ;         #(agent-12-lowstdev-mr 7.0 1 200 10)
   ;          #(agent-12-lowstdev-mr 9.0 1 200 10)

  ;           #(agent-12-lowstdev-mr 3.0 1 300 30)
 ;            #(agent-12-lowstdev-mr 5.0 1 300 30)
             ;#(agent-12-lowstdev-mr 7.0 1 300 30 15 15)
             ;#(agent-12-lowstdev-mr 5.0 1 300 30 30 15)
             ;#(agent-12-lowstdev-mr 5.0 1 300 30 10 10)
             ;#(agent-12-lowstdev-mr 4.0 1 300 30 10 20)
             ;#(agent-12-lowstdev-mr 3.0 1 300 30 40 20)
             ;#(agent-12-lowstdev-mr 2.0 1 300 30 40 40)
             ;#(agent-12-lowstdev-mr 2.0 1 300 30 60 15)
             ;#(agent-12-lowstdev-mr 2.0 1 300 30 60 30)

;             #(agent-14-learned-tree 10.0 300 30)


;             #(agent-12-lowstdev-mr 9.0 1 300 30)


;#(agent-11-market-make 0.03 100 200 3)

            ; #(agent-7-returns-quality-mr 1.00 20 400 1 "2" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 0.50 10 400 1 "3" ) ;; seems like a sweet spot
            ; #(agent-7-returns-quality-mr 1.15 10 400 1 "4" ) ;; seems like a sweet spot
;#(agent-8-lm 0.20 25 1 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.20 50 3 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.20 100 5 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.20 200 10 ) ;; seems like a sweet spot

                          ;#(agent-8-lm 0.10 25 1 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.10 50 3 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.10 100 5 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.10 200 10 ) ;; seems like a sweet spot

             ;             #(agent-8-lm 0.30 25 1 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.30 50 3 ) ;; seems like a sweet spot
             ;#(agent-8-lm 0.30 100 5 ) ;; seems like a sweet spot
;             #(agent-8-lm 0.30 200 10 ) ;; seems like a sweet spot
;#(agent-4-profit-taker 20 0.20 50 3 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 20 0.20 50 7 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 20 0.20 50 10 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 10 0.20 50 1 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 10 0.20 50 3 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 10 0.20 50 7 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 10 0.20 50 10 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 1 0.20 50 1 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 1 0.20 50 3 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 1 0.20 50 7 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 1 0.20 50 10 ) ;; seems like a sweet spot

             ;#(agent-4-profit-taker 10 0.20 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 0 0.20 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 30 0.30 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 20 0.30 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 10 0.30 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 0 0.30 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 30 0.10 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 20 0.10 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 10 0.10 50 ) ;; seems like a sweet spot
             ;#(agent-4-profit-taker 0 0.10 50 ) ;; seems like a sweet spot

             ;#(agent-4-profit-taker 40 0.20 60 )
             ;#(agent-4-profit-taker 50 0.20 70 )

             ;#(agent-4-profit-taker 30 0.20 20 )
             ;#(agent-4-profit-taker 40 0.20 30 )
             ;#(agent-4-profit-taker 50 0.20 40 )

              ;#(agent-4-profit-taker 30 0.20 20 )
             ;#(agent-4-profit-taker 40 0.20 30 )
             ;#(agent-4-profit-taker 50 0.20 40 )

;             #(agent-6-rare-trader 0  300 )
 ;            #(agent-6-rare-trader 10 200 )
  ;           #(agent-6-rare-trader 20 100 )

;agent-15-simple-momentum
;  " follow trend"
 ;            [zpp r-o max-n-trades dd pnl-target]

     ;        #(agent-15-simple-momentum 0.50 200 10 1000 5000 )
     ;        #(agent-15-simple-momentum 0.50 200 10 1000 5000 )
      ;       #(agent-15-simple-momentum 0.50 200 10 1000 5000 )
       ;      #(agent-15-simple-momentum 0.50 200 10 1000 5000 )

        ;     #(agent-15-simple-momentum 0.75 200 10 1000 5000 )
         ;    #(agent-15-simple-momentum 0.75 200 10 1000 5000 )
          ;   #(agent-15-simple-momentum 0.75 200 10 1000 5000 )
           ;  #(agent-15-simple-momentum 0.75 200 10 1000 5000 )

           ;  #(agent-15-simple-momentum 1.00 200 10 1000 5000 )
           ;  #(agent-15-simple-momentum 1.00 200 10 1000 5000 )
           ;  #(agent-15-simple-momentum 1.00 200 10 1000 5000 )
            ; #(agent-15-simple-momentum 1.00 200 10 1000 5000 )
;#(agent-15-simple-momentum 0.25 100 10 200 100 15 )
                                        ;#(agent-15-simple-momentum 0.25 100 10 200 100 25 )

             ;agent-15-simple-momentum

             ;[sharpe r-o max-n-trades dd rq-duration]

             ;#(agent-15-simple-momentum 0.25 100 10 100 35 )
             ;#(agent-15-simple-momentum 0.35 200 10 200 25 )
             ;#(agent-15-simple-momentum 0.45 300 10 300 45 )
             ;#(agent-15-simple-momentum 0.55 400 10 50 45 )
             ;#(agent-15-simple-momentum 0.55 300 10 50 45 )
             ;#(agent-15-simple-momentum 0.55 200 10 50 45 )
             ;#(agent-15-simple-momentum 0.10 100 10 200 10 )
             ;#(agent-15-simple-momentum 0.10 50 10 200 10 )
             ;#(agent-15-simple-momentum 0.10 20 10 200 10 )
             ;#(agent-15-simple-momentum 0.05 20 10 200 10 )

             ;#(agent-16-fernholz 0 200 10 "0-200-10" )
             ;#(agent-16-fernholz 3 200 10 "3-200-10" ) ;; 15 minutes
             ;#(agent-16-fernholz 6 200 10 "6-200-10" )
             ;#(agent-16-fernholz 8 200 10 "8-200-10" )

             ;#(agent-15-simple-momentum 0.25 100 10 200 100 45 )
           ;  #(agent-15-simple-momentum 0.25 100 10 1000 5000 )
           ;  #(agent-15-simple-momentum 0.25 100 10 1000 5000 )
           ;  #(agent-15-simple-momentum 0.25 100 10 1000 5000 )

           ;  #(agent-15-simple-momentum 0.75 100 10 1000 5000 )
            ; #(agent-15-simple-momentum 0.75 100 10 1000 5000 )
           ;  #(agent-15-simple-momentum 0.75 100 10 1000 5000 )
           ;  #(agent-15-simple-momentum 0.75 100 10 1000 5000 )

            ; #(agent-15-simple-momentum 1.00 100 10 1000 5000 )
            ; #(agent-15-simple-momentum 1.00 100 10 1000 5000 )
            ; #(agent-15-simple-momentum 1.00 100 10 1000 5000 )
            ; #(agent-15-simple-momentum 1.00 100 10 1000 5000 )
;#(agent-15-simple-momentum 0.20 300 10 200 50 )
])
