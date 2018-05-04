(ns matrix.indicators

  (:use [bigml.histogram.core])
  (:require (bigml.histogram.test [examples :as ex]))

  (:require
   [matrix.utils :as utils] [matrix.tickers :as tickers]
   [matrix.jutsudata :as jutsudata]

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
];   [matrix.tickers :as tickers]

   [matrix.indicators :as indicators])

  (:use clojure.pprint)

  (:gen-class))

;; idea to decide indicator distribution on the fly

(defn create-indicator-hist
  "return indicator instances on tickers and dates, typically assigned to indicator-state variable"
  [tickers] (zipmap (map keyword tickers)
                    (repeatedly (count tickers) create)))

(def distributions (create-indicator-hist tickers/huge-tickers))
(def distributions-last-return (create-indicator-hist tickers/huge-tickers))

;(pprint distributions)


;; so this is the function that operates on bar data
(defn returns
  "indicator has a header and data, so i know what it is"
  [indicator-state tic date]
  (let [price @(:price-bars @((indicator-state tic) date))]
    (conj (map utils/return-bps-col (partition 2 1 price)) 0)))

(defn returns-from-open [indicator-state tic date] (let [price @(:price @((indicator-state tic) date))
                                                         seq-no @(:seq-no @((indicator-state tic) date))

                                                         open @(:open @((indicator-state tic) date))
                                                         indicator-value (utils/return-bps  price open)]

                                                     {:header "returns-from-open" :data indicator-value}))

(defn returns-from-open-smooth [indicator-state tic date] (let [price-bars @(:price-bars @((indicator-state tic) date))
                                                                pb-smooth (reverse (utils/ema-all 10 (reverse price-bars)))

                                                                open (first pb-smooth)
                                                                indicator-value (utils/return-bps (last price-bars) open)]
                                                            {:header "returns-from-open-smooth" :data indicator-value}))

(defn returns-from-origin [indicator-state tic date duration] (let [price-bars (take-last duration @(:price-bars @((indicator-state tic) date)))
                                                                    open (first price-bars)
                                                                    indicator-value (utils/return-bps (last price-bars) open)]
                                                                {:header "returns-from-origin" :data indicator-value}))

(defn recent-returns [indicator-state tic date]  (let [price @(:price-bars @((indicator-state tic) date))
                                                       returns (conj (map utils/return-bps-col (partition 2 1 price)) 0)

                                                       indicator-value (last (map (partial utils/round2 2) (map #(reduce + (take-last 5 (take % returns)))  (range 1 (+ 1 (count returns))))))]

                                                   {:header "recent-returns" :data indicator-value}))

(defn stdev-ema [duration indicator-state tic date] (let [price @(:price-bars @((indicator-state tic) date))

                                                          returns (conj (map utils/return-bps-col (partition 2 1 price)) 0)
                                                          intermediate (utils/ema duration (map #(utils/sd (take-last 5 (take % returns)))  (range 1 (+ 1 (count  returns)))))

                                                          indicator-value (cond (nil? intermediate) Double/NaN :else intermediate)]
                                                      {:header "stdev-ema-20" :data indicator-value}))

(defn volume-ema [indicator-state tic date] (let [volume @(:volume-bars @((indicator-state tic) date))
                                                  intermediate (utils/ema 20 volume)
                                                  indicator-value (cond (nil? intermediate) Double/NaN :else intermediate)]

                                              {:header "volume-ema-20" :data indicator-value}))

(defn price-ema [duration indicator-state tic date]
  (let [prices @(:price-bars @((indicator-state tic) date))
        indicator-value
        (cond (nil? (utils/ema duration prices)) Double/NaN :else
              (utils/ema duration prices))]
    {:header (str "price-ema-" duration) :data indicator-value}))

(defn moving-dispersion
  "yes only if dispersion is moving, not if flat"
  [indicator-state tic date]
  (let [dispersion @(:dispersion-bars @((indicator-state tic) date))
        indicator-value
        (cond (< (count dispersion) 2) 0 :else (- (last dispersion) (last (drop-last dispersion))))]
    {:header "moving-dispersion" :data indicator-value}))

(defn pseudo-price-ema [duration indicator-state tic date]
  (let [prices @(:price-bars @((indicator-state tic) date))
        opens (repeat (count prices) @(:open @((indicator-state tic) date)))
        pseudo-prices (map utils/return-bps prices opens)

        indicator-value
        (cond (nil? (utils/ema duration pseudo-prices)) Double/NaN :else
              (utils/ema duration pseudo-prices))]
    {:header (str "pseudo-price-ema-" duration) :data indicator-value}))

(defn returns-quality [duration indicator-state tic date]
  (let [prices (take-last duration @(:price-bars @((indicator-state tic) date)))
        returns (conj (map utils/return-bps-col (partition 2 1 prices)) 0)

        indicator-value (first (utils/pnl-sharpe (map utils/sign returns)))]
    {:header "returns-quality" :data indicator-value}))

(defn bidask [indicator-state tic date]
  (let [askprice @(:askprice @((indicator-state tic) date))
        bidprice @(:bidprice @((indicator-state tic) date)) indicator-value (apply max [0.20 (- askprice bidprice)])]
    {:header "bidask" :data indicator-value}))

(defn p->pseudo
  "takes a coll and tranforms to psedusdo price in bps"
  [p]

  (cond (empty? p) nil :else (let [opens (repeat (count p) (first p))
                                   pseudo-prices (map utils/return-bps p opens)]
                               pseudo-prices)))

(defn linear-regression-slope
  "fit lm model to last N points and report slope value"
  [cutoff indicator-state tic date]

  (let [prices @(:price-bars @((indicator-state tic) date))
      ;prices-ema

      ;(comment (nil? (utils/ema duration prices )) Double/NaN :else (utils/ema duration prices ))
        prices-nonan (filter #(not (Double/isNaN %)) prices)
        N (count prices-nonan)

        indicator-value
        (cond (>= N cutoff)  ;; atleast 20+12 data points (32 mins)
              (let [points (reverse (take-last cutoff prices-nonan))
                    points-pseudo (p->pseudo points)
                    points-lm (map vector points-pseudo (range cutoff))
                    lm-slope (first (:coefs
                                     (stats/linear-model
                                      (map first points-lm)
                                      (map second points-lm)
                                      :intercept false)))]
                (- lm-slope))
              :else
              (identity Double/NaN))]

  ;(if (not (Double/isNaN indicator-value)) (insert! (tic distributions) indicator-value )  )
    {:header "linear-regression-slope" :data indicator-value :distribution (tic distributions)}))

(defn sharpe-from-distribution [indicator-state tic date]
  (let [dist (tic distributions)

        m (median dist)
        v (variance dist)
        sharpe (cond (or (nil? v) (zero? v)) 0.0 :else (/ m v))
        indicator-value (utils/round sharpe)]
    {:header "sharpe-from-distribution"  :data indicator-value}))

(defn lm-zscore [indicator-state tic date]
  (let [dist (tic distributions)
        value (:data (linear-regression-slope 10 indicator-state tic date))
        m (median dist)
        v (incanter/sqrt (variance dist))
        zscore (cond (or (nil? v) (zero? v)) 0.0 :else (/ (- value m) v))
        indicator-value (utils/round zscore)]
    {:header "lm-zscore"  :data indicator-value}))

(defn variance-from-distribution [indicator-state tic date]
  (let [dist (tic distributions)

        m (median dist)
        v (variance dist)
        v-clean (cond (or (nil? v) (zero? v)) 0.0 :else v)
        indicator-value (utils/round v-clean)]
    {:header "variance-from-distribution"  :data indicator-value}))
;(insert! (:BUD distributions) Double/NaN )


(defn linear-regression-slope-diff
  "fit lm model to last N points and report slope value"
  [indicator-state tic date]

  (let [prices @(:price-bars @((indicator-state tic) date))

        prices-nonan (filter #(not (Double/isNaN %)) prices)
        N (count prices-nonan)

        indicator-value
        (cond (>= N 5)  ;; atleast 20+12 data points (32 mins)
              (let [cutoff N
                    points (reverse prices-nonan)
                    points-pseudo (p->pseudo points)
                    points-lm (map vector points-pseudo (range cutoff))
                    lm-slope (first (:coefs
                                     (stats/linear-model
                                      (map first points-lm)
                                      (map second points-lm)
                                      :intercept false)))

                    points-prev (reverse (drop-last prices-nonan))
                    points-pseudo-prev (p->pseudo points-prev)
                    points-lm-prev (map vector points-pseudo-prev (range cutoff))
                    lm-slope-prev (first (:coefs
                                          (stats/linear-model
                                           (map first points-lm-prev)
                                           (map second points-lm-prev)
                                           :intercept false)))]
                (- lm-slope-prev lm-slope))
              :else
              (identity Double/NaN))]
    {:header (str "linear-regression-slope-diff-" (name tic)) :data indicator-value}))

;; high r2, slope negative, and slope diff >
;; 0.75 length 100 for lm-diff
;; 0.96 length for r-square


(defn returns-from-open-zscore [indicator-state tic date portfolio duration] (let [all-returns (map #(:data (returns-from-origin indicator-state % date duration)) (map keyword portfolio))
                                                                                   this-return (:data (returns-from-origin indicator-state tic date duration))
                                                                                   indicator-value (utils/zscore2 this-return all-returns)]
                                                                               {:header "returns-from-open-zscore" :data indicator-value}))

(def capital 1000000)
(defn allocation [price] (utils/round (/ (/ capital 40) price)))

(defn fernholz-target
  "what is target inventory for equal portfolio"
  [when-to-peg indicator-state tic date]
  (let [prices @(:price-bars @((indicator-state tic) date))
        fernholz-inventories (map allocation prices)
        indicator-value (cond (<= (count prices) when-to-peg) Double/NaN
                              :else (let [starting-position (nth fernholz-inventories when-to-peg)]
                                      (- (last fernholz-inventories) starting-position)))]
    {:header "fernholz-target" :data indicator-value}))

(comment fernholz-target-2
         "what is target inventory for equal portfolio"
         [indicator-state tic date]
         (let [prices @(:price-bars @((indicator-state tic) date))
               fernholz-inventories (map allocation prices)
               indicator-value (cond (<= (count prices) when-to-peg) Double/NaN
                                     :else (let [starting-position (nth fernholz-inventories when-to-peg)]
                                             (- (last fernholz-inventories) starting-position)))]
           {:header "fernholz-target" :data indicator-value}))

(defn cumulative-relative-volume [indicator-state tic date] (let [volume @(:volume-bars @((indicator-state tic) date))

                                                                  seq-no @(:seq-no @((indicator-state tic) date))
                                                                  adj-factor (/ 390 (+ 1 seq-no))

                                                                  avg-volume-per-day (stats/mean (:v (tickers/ohlcv (name tic))))
                                                                  indicator-value (cond (nil? avg-volume-per-day) Double/NaN :else
                                                                                        (* adj-factor (reduce + (map #(/ % avg-volume-per-day) volume))))] {:header "cumulative-relative-volume" :data indicator-value}))

(defn l1-price [indicator-state tic date] (let [indicator-value @(:price @((indicator-state tic) date))
                                                seq-no @(:seq-no @((indicator-state tic) date))] {:header "l1-price"
                                                                                                  :data indicator-value}))

(defn ticker-time [indicator-state tic date] (let [indicator-value @(:time @((indicator-state tic) date))] {:header "ticker-time"
                                                                                                            :data indicator-value}))

(defn l1-volume [indicator-state tic date] (let [indicator-value @(:volume @((indicator-state tic) date))] {:header "l1-volume"
                                                                                                            :data indicator-value}))

(defn trade-time-lapse
  "inverse of time diff, large means someone is trading actively"
  [indicator-state tic date]
  (let [ttl @(:trade-time-lapse @((indicator-state tic) date))

        indicator-value (cond (zero? ttl) 0.0
                              :else (/ 1 ttl))]
    {:header "trade-time-lapse" :data indicator-value}))

(defn autocorr [indicator-state tic date] (let [prices @(:price-bars @((indicator-state tic) date))
                                                lag-prices (utils/lag 1 prices)
                                                diff  (rest (map - prices lag-prices))
                                                indicator-value (cond (< (count diff) 2)  Double/NaN :else
                                                                      ;(last diff)
                                                                      (stats/correlation (drop-last diff) (rest diff)))]
                                            {:header "autocorr" :data (utils/round2 4 indicator-value)}))

(defn last-return [duration indicator-state tic date] (let [prices @(:price-bars @((indicator-state tic) date))
                                                            lag-prices (utils/lag 1 prices)
                                                            diff  (rest (map - prices lag-prices))

                                                            indicator-value (cond (< (count diff) duration)  Double/NaN :else
                                                                                  ;(- (last prices) (last (drop-last prices)))
                                                                                  (reduce + (take-last duration diff)))] {:header "last-return" :data (utils/round2 4 indicator-value)}))

(defn last-return-zscore [indicator-state tic date]
  (let [dist (tic distributions-last-return)
        value (:data (last-return indicator-state tic date))
        m (median dist)
        v (incanter/sqrt (variance dist))
        zscore (cond (or (nil? v) (zero? v)) 0.0 :else (/ (- value m) v))
        indicator-value (utils/round zscore)]
    {:header "last-return-zscore"  :data indicator-value}))

(defn simple-trend [duration indicator-state tic date]
  (let [prices @(:price-bars @((indicator-state tic) date))
        opens (repeat (count prices) @(:open @((indicator-state tic) date)))
        pprices (utils/csum (map utils/return-bps prices opens)) indicator-value
        (cond (nil? (utils/ema duration pprices)) Double/NaN :else
              (- (last pprices) (utils/ema duration pprices)))]
    {:header (str "simple-trend-" duration) :data (utils/round indicator-value)}))
(defn position-to-take [capital indicator-state tic date]
  (let [price @(:price @((indicator-state tic) date))
        value (cond (zero? price) 0 :else (utils/round2 0 (/ capital price))) indicator-value
        (cond (nil? value) Double/NaN :else
              value)]
    {:header (str "position-to-take") :data indicator-value}))

(defn pnl-indicator [indicator-state tic date]
  (let [pnl @(:pnl @((indicator-state tic) date))
        indicator-value
        (cond (nil? pnl) Double/NaN :else
              pnl)]
    {:header (str "pnl-indicator") :data indicator-value}))

(defn stdev-price [duration indicator-state tic date] (let [price @(:price-bars @((indicator-state tic) date))
                                                            rel-prices (take-last duration price)
                                                            intermediate (stats/sd rel-prices)

                                                            indicator-value (cond (or (< (count rel-prices) duration) (nil? intermediate)) Double/NaN :else intermediate)]
                                                        {:header "stdev-price" :data indicator-value}))

(defn r-square
  "fit lm model to last N points and report slope value"
  [indicator-state tic date]

  (let [prices (reverse @(:price-bars @((indicator-state tic) date)))
      ;prices-ema

      ;(comment (nil? (utils/ema duration prices )) Double/NaN :else (utils/ema duration prices ))
        prices-nonan (filter #(not (Double/isNaN %)) prices)
        N (count prices-nonan)

        indicator-value
        (cond (>= N 5)  ;; atleast 20+12 data points (32 mins)
              (let [points prices-nonan
                    points-pseudo (p->pseudo points)
                    points-lm (map vector points-pseudo (range N))
                    r2 (:r-square
                        (stats/linear-model
                         (map first points-lm)
                         (map second points-lm)))]
                r2)
              :else
              (identity Double/NaN))]
    {:header  (str "r-square-" (name tic)) :data indicator-value}))

(defn lm-intercept-slope
  "fit lm model to last N points and report slope value, reverses price series"
  [indicator-state tic date]

  (let [prices @(:price-bars @((indicator-state tic) date))
      ;prices-ema

      ;(comment (nil? (utils/ema duration prices )) Double/NaN :else (utils/ema duration prices ))
        prices-nonan (filter #(not (Double/isNaN %)) prices)
        N (count prices-nonan)

        indicator-value
        (cond (>= N 5)  ;; atleast 20+12 data points (32 mins)
              (let [points (reverse prices-nonan)
                    points-pseudo (p->pseudo points)
                    points-lm (map vector points-pseudo (range N))
                    coefs (:coefs
                           (stats/linear-model
                            (map first points-lm)
                            (map second points-lm)
                            :intercept false))]
                (- (first coefs))
              ;coefs
)
              :else
              (identity Double/NaN))]
    {:header (str "lm-intercept-slope-" (name tic)) :data indicator-value}))

(comment

  #(and (> (:data (r-square i-state (keyword %) :20180319)) 0.96)
        (> (:data (linear-regression-slope-diff i-state (keyword %) :20180319)) 0.50)) ticker-coll)
