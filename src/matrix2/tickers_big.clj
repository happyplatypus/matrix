(ns matrix.tickers_big
  (:require
   [matrix.utils :as u]

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
(do (def HOME u/HOME)
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

;;;;;;;; functions around date decision

(do (defn formatlocal [n offset]
      (let [nlocal (tt/to-time-zone n (tt/time-zone-for-offset offset))]
        (tf/unparse (tf/formatter-local "yyyy-MM-dd hh:mm:ss aa")
                    nlocal)))

    (defn currentTime []
      (formatlocal (tt/now) -5))

    (defn pad_ [x] (if (= 2 (count (str x))) (identity x) (str "0" x)))

    (defn convert_ [date]
      (str
       (tt/year date)
       (pad_ (tt/month date))
       (pad_ (tt/day date))))

    (defn convert-javatime-to-yyyymmdd [datetime]
      (tt/date-time (read-string (subs datetime

                                       0 4)) (. Integer parseInt (subs datetime 5 7)) (. Integer parseInt (subs datetime 8 10))))

    (def tt_date_  (tt/date-time (read-string (subs (currentTime) 0 4)) (. Integer parseInt (subs (currentTime) 5 7)) (. Integer parseInt (subs (currentTime) 8 10))))

;;asofdata logic
                                        ;(def tt_date_ (tt/date-time 2017 6 4 )  )

    (currentTime)
    (println "today is..")
    (println tt_date_)
    (identity tt_date_)

    (def tt_date_1 (tt/minus tt_date_ (tt/days 1)))
    (def tt_date_2 (tt/minus tt_date_ (tt/days 2)))
    (def tt_date_3 (tt/minus tt_date_ (tt/days 3)))
    (def tt_date_4 (tt/minus tt_date_ (tt/days 4)))
    (def tt_date_5 (tt/minus tt_date_ (tt/days 5)))

    (def tt_date_14 (tt/minus tt_date_ (tt/days 14)))
    (def tt_date_30 (tt/minus tt_date_ (tt/days 30)))
    (def tt_date_96 (tt/minus tt_date_ (tt/days 96)))

    (def startDate (convert_ tt_date_96))
    (identity startDate)

;; does all logic of weekend adjusting
    (def today_
      (cond (pr/saturday? tt_date_) (identity  (str
                                                (tt/year tt_date_1)
                                                (pad_ (tt/month tt_date_1))
                                                (pad_ (tt/day tt_date_1))))
            (pr/sunday? tt_date_) (identity  (str
                                              (tt/year tt_date_2)
                                              (pad_ (tt/month tt_date_2))
                                              (pad_ (tt/day tt_date_2))))
            :else (convert_ tt_date_)))

    (identity today_)
    (def today_int (atom (read-string today_)))
;(def today_int 20170928)
                                        ;(identity yesterday_)


    (def yesterday_
  ;; if saturday this is t-2
  ;; if sunday this t-3
  ;; else t-1
      (cond (pr/saturday? tt_date_) (identity  (str
                                                (tt/year tt_date_2)
                                                (pad_ (tt/month tt_date_2))
                                                (pad_ (tt/day tt_date_2))))
            (pr/sunday? tt_date_) (identity  (str
                                              (tt/year tt_date_3)
                                              (pad_ (tt/month tt_date_3))
                                              (pad_ (tt/day tt_date_3))))

            (pr/monday? tt_date_) (identity  (str
                                              (tt/year tt_date_3)
                                              (pad_ (tt/month tt_date_3))
                                              (pad_ (tt/day tt_date_3))))
            :else (convert_ tt_date_1)))

;;;;;;; functions around date decision


;; find advs


    (defn dailyBars
      [date1 date2 tic]
      (let [tail (str "&historyType=1&beginTime=" date1 "093000" "&endTime=" date2 "160000")]
        (:body (client/get (str "http://localhost:5000/barData?symbol=" tic tail)))))

    (defn getBars2
      [tic date minutes]
      (let [tail (str "&historyType=0&intradayMinutes=" minutes "&beginTime=" date "093000" "&endTime=" date "160000")]
        (:body (client/get (str "http://localhost:5000/barData?symbol=" tic tail)))))
    (defn getReturns2
      [date minutes tic]
      (let
       [data1 (getBars2 tic date minutes)
        data2 (map #(str/split % #",") (str/split data1  #"\r\n"))
        price (map read-string (map #(nth % 4) data2))
        logprice (map incanter/log price)
        returns  (map int (map #(* % 10000) (map - (rest logprice) (drop-last logprice))))] (identity returns)))

                                        ;(def startDate (convert_ tt_date_30 ))
                                        ;(identity startDate)


    (def apple-data (clojure.string/split (dailyBars startDate today_ "AAPL") #"\r\n"))
    (def N (count apple-data))
    (identity N)

;;;find advs

    (defn ohlcv-dumb [tic]
      (let [apple-data (clojure.string/split (dailyBars startDate today_ tic) #"\r\n")
            N-local (count apple-data)
            data (map #(clojure.string/split % #",") apple-data)
            close-price (map read-string (map  #(nth % 4) data))
            open-price  (map read-string (map #(nth % 1) data))
            high-price  (map read-string (map  #(nth % 2) data))
            low-price  (map read-string (map  #(nth % 3) data))
            volumes  (map read-string (map  #(nth % 5) data))
            avg-price (stats/mean close-price)
            stock-type (cond (<= avg-price 15) "small"
                             (>= avg-price 90) "large"
                             :else "mid")
            adv-in-millions (u/round2 2 (/ (stats/mean (map * volumes close-price)) 1E6))
            d [0]  ;; dummy data
            good-data {:aprice avg-price :type stock-type :aticker tic :status "Active" :o open-price :c close-price :h high-price :l low-price :v volumes :adv adv-in-millions}

            bad-data {:aprice 100 :type stock-type :aticker tic :status "Inactive" :o d :c d :h d :l d :v d :adv 0}] (if (< N-local N) (identity bad-data) (identity good-data))))

    (defn ohlcv-dumb2 [tic]
      (let [apple-data (clojure.string/split (dailyBars startDate today_ tic) #"\r\n")
            N-local (count apple-data)
            data (map #(clojure.string/split % #",") apple-data)
            close-price (map read-string (map  #(nth % 4) data))
            dates-long (map read-string (map  #(nth % 0) data))
            dates (map #(/ % 1000000) dates-long)

            open-price  (map read-string (map #(nth % 1) data))
            high-price  (map read-string (map  #(nth % 2) data))
            low-price  (map read-string (map  #(nth % 3) data))
            volumes  (map read-string (map  #(nth % 5) data))
            avg-price (stats/mean close-price)
            stock-type (cond (<= avg-price 15) "small"
                             (>= avg-price 90) "large"
                             :else "mid")
            adv-in-millions (u/round2 2 (/ (stats/mean (map * volumes close-price)) 1E6))
            d [0]  ;; dummy data
            good-data {:aprice avg-price :type stock-type :aticker tic :status "Active" :o open-price :c close-price :h high-price :l low-price :v volumes :adv adv-in-millions :dates dates}] (if (< N-local N) (identity nil) (identity good-data))))

                                        ;(map #(take 10 (second %)) (ohlcv-dumb2 "TSLA"))


                                        ;(map class (map second (ohlcv-dumb2 "TSLA")))

    (def ohlcv (clojure.core/memoize ohlcv-dumb2))
    (defn recent-dates []
      (:dates (ohlcv "AAPL")))

    (defn ohlcv-date [date tic]
      (let [data  (ohlcv tic)
            out-data (cond (nil? data) (identity nil)
                           :else
                           (let [close-price (map second (filter #(< (first %) date)  (map vector (:dates data)  (:c data))))
                                 open-price (map second (filter #(< (first %) date)  (map vector (:dates data)  (:o data))))
                                 high-price (map second (filter #(< (first %) date)  (map vector (:dates data)  (:h data))))
                                 low-price (map second (filter #(< (first %) date)  (map vector (:dates data)  (:l data))))
                                 volumes (map second (filter #(< (first %) date)  (map vector (:dates data)  (:v data))))
                                 adv-in-millions (u/round2 2 (/ (stats/mean (map * volumes close-price)) 1E6))
                                 dates (map second (filter #(< (first %) date)  (map vector (:dates data)  (:dates data))))
                                 avg-price (stats/mean close-price)]
                             {:aprice avg-price :type (:stock-type data) :aticker tic :status "Active" :o open-price :c close-price :h high-price :l low-price :v volumes :adv adv-in-millions :dates dates}))]
        (identity out-data))));;end do


;;;; finished tracking all eligible tickers for trading, now see earnings page.


(defn ema [HL values]
  (reductions (fn [running v]
                (let [f (/ 2 (+ 1 HL))
                      one-minus-F (- 1 f)] ;naming intermediate results can help with the readability of non-associative operators.
                  (+ (* f v)
                     (* one-minus-F running))))
              values))

;(def ohlcv-data (map ohlcv (take 10 (concat (map first filter-data2-nyse)  (map first filter-data2-nasdaq)))))


;; being rank indicators


(defn trend
  "simple trend using return series
indicators are maps with header and dates
data is always adapted, ie data with 20160729 is avl start of 20170729
"
  [fast slow aticker]
  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [p (:c tech)
                                        dates (rest (:dates tech))
                                        rets  (map u/return_bps (rest p)  (drop-last p))
                                        adj-price (u/csum (u/winsorize 2 rets))
                                        trend (map - (ema fast adj-price) (ema slow adj-price))]
                ;(count dates)
                                    {:data (utils/lag 1  (map (partial u/round2 2)  trend))
                                     :dates dates
                                     :header "trend"}))))

(defn simple-trend-live
  "simple trend using return series
indicators are maps with header and dates
live indicators are to be used for intraday ranking
ie what is the top price-open stock now?

"
  [slow aticker]
  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [p (:c tech)
                                        dates (rest (:dates tech))
                                        ema-p (ema slow p)

                                        rets  (map u/return_bps p  ema-p)]
                ;(count dates)
                                    {;:data (map (partial u/round2 2)  trend)
                                     :data ema-p
                                     :dates dates
                                     :header "trend"}))))

(defn movement-live
  "now minus yesterdays close
indicators are maps with header and dates
live indicators are to be used for intraday ranking
ie what is the top price-open stock now?

"
  [aticker]
  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [p (:c tech)
                                        dates (:dates tech)
                                        lag-p (u/lag 1 p)

                                        rets  (map u/return_bps p  lag-p)]
                ;(count dates)
                                    {;:data (map (partial u/round2 2)  trend)
                                     :data rets
                                     :dates dates
                                     :header "trend"}))))

(defn intraday-movement-live
  "now minus yesterdays close
indicators are maps with header and dates
live indicators are to be used for intraday ranking
ie what is the top price-open stock now?
"
  [aticker]
  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [c (:c tech)
                                        o (:o tech)
                                        dates (:dates tech) rets  (map u/return_bps c o)]
                ;(count dates)
                                    {;:data (map (partial u/round2 2)  trend)
                                     :data rets
                                     :dates dates
                                     :header "intraday-movement-live"}))))

;(u/view-indicator (movement-live "MTW"))
;(ohlcv "MTW")

(defn trend2
  "intraday trend - ignore moves across days"
  [fast slow aticker]

  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [c (:c tech)
                                        o (:o tech)
                                        rets  (map u/return_bps c o)
                                        adj-price (u/csum (u/winsorize 2 rets))
                                        trend (map - (ema fast adj-price) (ema slow adj-price))] (map (partial u/round2 2)  trend)))))

;(identity (trend 1 3 "EGBN"))

(defn getBars2
  [tic date minutes]
  (let [tail (str "&historyType=0&intradayMinutes=" minutes "&beginTime=" date "093000" "&endTime=" date "160000")
        data (:body (client/get (str "http://localhost:5000/barData?symbol=" tic tail)))
        test (first (first (map #(str/split % #",") (str/split data  #"\r\n"))))
        answer (cond (= "00000000000000" test) (identity nil) :else (identity data))]
    (identity answer)))

(defn last-close [aticker]
  "which ones broke today?"
  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [c (:c tech)]
                ;(last (drop-last c))
                                    (last c)))))

;(last-close "AAPL")

(defn returns-from-yesterday [date yesterday_ minutes tic]
  (let
   [data1 (getBars2 tic date minutes)
    lc    (last-close tic)
    return-from-yesterday (cond (or (zero? lc) (nil? lc) (nil? data1)) (identity nil)
                                :else

                                (let [data2 (map #(str/split % #",") (str/split data1  #"\r\n"))
                                      price (map read-string (map #(nth % 4) data2))
                                      logprice (map incanter/log price)
   ;;; now see if last days close to now is actually significant
                                      return (- (last logprice) (incanter/log lc))] (u/round2 4 return)
;(identity data1)
))]
    (identity return-from-yesterday)))

;(returns-from-yesterday today_ yesterday_ 1 "DBD")

;(last-close "AAPL")
;(getBars2 "AAPL" today_ 1)


;(trend 8 16 "JNJ")


;(conj [1 2] '())

;(println (filter #(< % (- 3)) (zscore rets)))


;;test for winsorize


(defn ATR-bps [aticker]
  "coll of ATR numbers"
  (let  [tech (ohlcv aticker)
         h (cond (nil? tech) nil :else
                 (:h tech))
         l (cond (nil? tech) nil :else
                 (:l tech))
         c (cond (nil? tech) nil :else
                 (:c tech))]
    (if (nil? h)

      (identity nil)

      (let [hl (rest (map incanter/abs (map u/return_bps h l)))
            first-day-atr (first (map incanter/abs (map u/return_bps h l)))
            hc (map incanter/abs (map u/return_bps (rest h) (drop-last c)))
            lc (map incanter/abs (map u/return_bps (rest l) (drop-last c)))]

        (map u/winsorize-returns (conj (map #(apply max %) (map vector hl hc lc))  first-day-atr))
;(identity tech)
))))

;(conj (ATR-bps "AAPL") -99 )

(defn close-open-bps [aticker]
  "coll of close minus open in bps"
  (let  [tech (ohlcv aticker)
         o (cond (nil? tech) nil :else
                 (:o tech))
         c (cond (nil? tech) nil :else
                 (:c tech))]
    (if (nil? o)
      nil (map u/winsorize-returns (map incanter/abs (map u/return_bps c o))))))

(defn volume-change-bps [aticker]
  "coll of"
  (let  [tech (ohlcv aticker)
         v (cond (nil? tech) nil :else
                 (:v tech))
         lag-v (utils/lag 1 v)]
    (if (nil? v)
      nil (map u/winsorize-returns (map incanter/abs (map u/return_bps v lag-v))))))

; (:c (ohlcv-date 20171102 "EXAS"))

(defn ATR-bps-show [date aticker]
  "coll of ATR numbers"
  (let  [tech (ohlcv-date date aticker)
         h (cond (nil? tech) nil :else
                 (:h tech))
         l (cond (nil? tech) nil :else
                 (:l tech))
         c (cond (nil? tech) nil :else
                 (:c tech))]
    (if (nil? h)

      (identity nil)

      (let [hl (rest (map incanter/abs (map u/return_bps h l)))
            hc (map incanter/abs (map u/return_bps (rest h) (drop-last c)))
            lc (map incanter/abs (map u/return_bps (rest l) (drop-last c)))]

        (map vector hl hc lc)
;(identity tech)
))))

(defn ATR-bps-T1
  "which atr dropped the most yesterday, lets assume that goes back up"
  ([date aticker]
   (let  [tech (ohlcv-date date  aticker)
          h (cond (nil? tech) nil :else
                  (:h tech))
          l (cond (nil? tech) nil :else
                  (:l tech))
          c (cond (nil? tech) nil :else
                  (:c tech))]
     (if (nil? h)

       (identity nil)

       (let [hl (rest (map incanter/abs (map u/return_bps h l)))
             hc (map incanter/abs (map u/return_bps (rest h) (drop-last c)))
             lc (map incanter/abs (map u/return_bps (rest l) (drop-last c)))
             ATR (map #(apply max %) (map vector hl hc lc))]
         (u/round2 0 (- (last ATR) (last (drop-last ATR))))))))
  ([aticker]
   (let  [tech (ohlcv aticker)
          h (cond (nil? tech) nil :else
                  (:h tech))
          l (cond (nil? tech) nil :else
                  (:l tech))
          c (cond (nil? tech) nil :else
                  (:c tech))]
     (if (nil? h)

       (identity nil)

       (let [hl (rest (map incanter/abs (map u/return_bps h l)))
             hc (map incanter/abs (map u/return_bps (rest h) (drop-last c)))
             lc (map incanter/abs (map u/return_bps (rest l) (drop-last c)))
             ATR (map #(apply max %) (map vector hl hc lc))]

         (u/round2 0 (- (last ATR) (last (drop-last ATR))))
        ;; this should be negative for turbulent stocks that have stabilized
)))))

;(ATR-bps-oc 1 "ZGNX")


;(u/lPlot2 (ATR-bps "IBTX")  (ATR-bps "IBTX")  )


(defn stdev-adj-trend [fast slow aticker]
  (let  [tech (ohlcv aticker)
         s (cond (nil? tech) nil :else
                 (stats/sd (:c (ohlcv aticker))))] (if (or (nil? s) (zero? s)) (identity nil) (map #(/ % s) (trend fast slow aticker)))))

(defn stdev-adj-trend2 [fast slow aticker]
  (let  [tech (ohlcv aticker)
         s (cond (nil? tech) nil :else
                 (stats/sd (:c (ohlcv aticker))))] (if (or (nil? s) (zero? s)) (identity nil) (map #(/ % s) (trend2 fast slow aticker)))))

;(ATR-bps-oc "EDIT")

;(normalize (stdev-adj-trend 8 24 "SYT"))

;(reverse nil)
(defn consolidated-trend [aticker]
  (let [sum-of-trends (map + (reverse (u/normalize (stdev-adj-trend 8 24 aticker)))
                           (reverse (u/normalize (stdev-adj-trend 16 48 aticker)))
                           (reverse (u/normalize (stdev-adj-trend 32 96 aticker))))]
    (if (empty? sum-of-trends) (identity nil) (map (partial u/round2 0)  (reverse sum-of-trends)))))

(defn consolidated-trend2 [aticker]
  "intraday"
  (let [sum-of-trends (map + (reverse (u/normalize (stdev-adj-trend2 8 24 aticker)))
                           (reverse (u/normalize (stdev-adj-trend2 16 48 aticker)))
                           (reverse (u/normalize (stdev-adj-trend2 32 96 aticker))))]
    (if (empty? sum-of-trends) (identity nil) (map (partial u/round2 0)  (reverse sum-of-trends)))))

;(consolidated-trend "BAOA")


(defn prices-ema [days aticker]
  (let [tech (ohlcv aticker)]
    (ema days (:c tech))))

(defn prices [aticker]
  (let [tech (ohlcv aticker)]
    (:c tech)))

(defn ema-slope [days aticker]
  (let [tech (ohlcv aticker)
        EMA     (ema days (:c tech))]
    (u/sign (stats/median (map - (rest EMA) (drop-last EMA))))))

(defn view-prices [aticker]
  (u/lPlot3 (prices-ema 14 aticker)  (prices-ema 7 aticker)  (prices aticker)))

(defn current-price [aticker]
  "tell me close price tom based on recent n day slope"
  (let [tech (ohlcv aticker)]
    (last (:c tech))))

(defn forecast-slope [days aticker]
  "tell me close price tom based on recent n day slope"
  (let [tech (ohlcv aticker)
        EMA     (ema days (:c tech))]
    (+ (last EMA) (last (map - (rest EMA) (drop-last EMA))))))

;(trend 14 "RH")
;(ema-slope 14 "RH")

;(view-prices "TSLA")
;(forecast-slope 14 "TSLA")
;(current-price "TSLA")
(defn intraday [aticker] (let [data (ohlcv aticker)
                               returns (u/round2 2 (/ (last (map - (:c data) (:o data))) (:aprice data)))]
                           (identity returns)))

(defn ten-day-stdev [aticker] (let [data (ohlcv aticker)
                                    stdev (u/round2 2 (stats/sd (take-last 10 (:c data))))]
                                (identity stdev)))

(defn intraday-movement [aticker] (let [data (ohlcv aticker)
                                        avg-move (u/round2 2 (stats/mean (map u/abs (map - (:c data) (:o data)))))]
                                    (identity avg-move)))

(defn print-tails [k col]
  (clojure.pprint/pprint (take k col))
  (clojure.pprint/pprint (take-last k col)))

(defn take-tails [k col]
  (concat (take k col)  (take-last k col)))

(def adv-map (map vector tickers (map :adv (map ohlcv tickers))))
;(print-tails (sort-by second adv-map))


(comment
  (def ticlist (map first (filter #(> (second %) adv-cutoff-millions) adv-map)))
  (spit  (str  HOME "/data/ticlist.pickle") (pr-str ticlist)))

;(def ticlist (read-string (slurp  (str  HOME "/data/ticlist.pickle" ) )))


;(def intraday-returns (map vector ticlist        (map intraday ticlist       )))

;(ohlcv "VWR")
;(print-tails (sort-by second intraday-returns))
;(def old-one (sort-by second intraday-returns))

;(print-tails (sort-by second adv-map))

;(filter #(contains? (set liquid-tickers) (first %) )   )
(def mid-tickers (map :aticker (filter #(= "mid" (:type %)) (map ohlcv tickers))))
;(def small-tickers (map :aticker (filter #(= "small" (:type %)) (map ohlcv tickers))))
(def large-tickers (map :aticker (filter #(= "large" (:type %)) (map ohlcv tickers))))

(def small-tickers (map :aticker (filter #(= "small" (:type %)) (map ohlcv tickers))))

;(take 10 large-tickers)
;(def ten-day-stdev-map (map vector mid-tickers (map ten-day-stdev mid-tickers)))
;(print-tails 20 (sort-by second ten-day-stdev-map))


;; good ticker logic

(def mc-cutoff 1000E6)
(def price-cutoff 10.0)
(def price-cutoff-hi 120.0)
(def adv-cutoff-millions 20.0)

(defn good-ticker? [aticker]
  (let [this-data (ohlcv aticker)
        result (cond (nil? this-data) (identity false)
                     :else
                     (let [this-status (:status this-data)
                           this-adv (:adv this-data)
                           this-name (stock-names aticker)
                           this-close (last (:c this-data))]
                       (if (or  (zero? this-close)  (nil? this-name) (not= "Active" this-status) (or (zero? this-adv) (nil? this-adv))
                                (< this-adv adv-cutoff-millions)
                                (< this-close price-cutoff)
                                (>= this-close price-cutoff-hi))

                         (identity false) (identity true))))]
    (identity result)))
;(println (ohlcv "BR"))
;(stock-names "AZPN")
;(good-ticker? "BR")


(def clean-ticnames-mid (filter good-ticker? mid-tickers))
;(count clean-ticnames-mid)
(def clean-ticnames-large (filter good-ticker? large-tickers))
;(count clean-ticnames-large)
(def clean-ticnames-small (filter good-ticker? small-tickers))
;(count clean-ticnames-small)
;(println small-tickers)
(def clean-ticnames (concat clean-ticnames-small  clean-ticnames-mid  clean-ticnames-large))
;(def trend-map (map vector mid-tickers (map consolidated-trend2 clean-ticnames ) ))

;(println trend-map)
(count clean-ticnames)

;; define a ranking to take a function and return a list of 20 stock
(defn adapt-indicator-data [date indicator-data]
  (second (last (filter #(<= (first %) date) (map vector (:dates indicator-data) (utils/lag 1 (:data indicator-data)))))))

(defn rank-simple-trend-live [ticnames date]
  (let [to-sort (map vector ticnames (map #(adapt-indicator-data date (simple-trend-live 10  %))  ticnames))
        out (take 20 (reverse (sort-by #(-> % second incanter/abs) to-sort)))]
    out))

(defn rank-intraday-movement-live [ticnames date]
  (let [to-sort (map vector ticnames (map #(adapt-indicator-data date (intraday-movement-live %))  ticnames))
        out (map first (take 5 (reverse (sort-by #(-> % second incanter/abs) to-sort))))]
    out))

(rank-simple-trend-live clean-ticnames-mid 20171205)

;; this seems like a good one
(rank-intraday-movement-live clean-ticnames-mid 20171204)
(rank-intraday-movement-live clean-ticnames-small 20171204)
(rank-intraday-movement-live clean-ticnames-large 20171204)

;;clean up this file for quick analysis from cmdline

(def N 60)
(def dates (take-last N (recent-dates)))
(def above (map #(rank-intraday-movement-live clean-ticnames-small %)  (take-last N (recent-dates))))
(identity above)
(identity dates)
(def above (map #(vector (str/join "," %)) above))
(def token (u/random-word))
(def above (map conj above (repeat (count dates) token)))
;(map cons dates above)

(def rank-data (map utils/coll->string2 (map cons dates above)))
(identity rank-data)
(maxp #(spit (str "/home/puru/Dropbox/data/rank-" token ".txt") % :append true) rank-data)

(comment (def advs (map vector clean-ticnames-mid (map adv clean-ticnames-mid)))

         (def most-liquid (map first (take-last 20 (sort-by second advs))))
         (spit "/home/puru/Dropbox/data/most-liquid-tickers.data" (pr-str most-liquid))

         (spit "/home/puru/Dropbox/data/recent-dates.data" (pr-str (:dates (ohlcv (first most-liquid)))))

         (def small-ohlcv-data (zipmap (map keyword clean-ticnames-small) (map ohlcv clean-ticnames-small)))

;; goal is to predict close-open
;; assemble factors that can help
         (zipmap (map keyword clean-ticnames-mid) (map #(-> % consolidated-trend2) clean-ticnames-mid))
         (take 10 (reverse (sort-by second (zipmap (map keyword clean-ticnames-mid) (map #(-> % ATR-bps last) clean-ticnames-mid)))))

         (map #(-> % consolidated-trend2) clean-ticnames-mid)
         (take 10 (reverse (sort-by second (zipmap (map keyword clean-ticnames-mid) (map #(-> % ATR-bps last) clean-ticnames-mid)))))

         (map (juxt #(-> % consolidated-trend2) #(-> % ATR-bps)) clean-ticnames-mid)
         (incanter/to-dataset {:a [1 2 3] :b [1 2 3]})

         (def aticker "EDIT")
         (defn factor-data [aticker] (incanter/to-dataset {:ctrend (drop-last (vec (consolidated-trend2 aticker)))
                                                           :atr (drop-last (vec (ATR-bps aticker)))
                                                           :close-open (rest (vec (close-open-bps aticker)))
                                                           :ticker
                                                           (vec (repeat (count (rest (close-open-bps aticker))) aticker))}))

         (defn factor-data2
           "wrong, dont use this...target format of data is [[] [] [] [] ] for each date "
           [aticker]
           {:data
            (map vector
                 (drop-last (vec (consolidated-trend2 aticker)))
                 (drop-last (vec (ATR-bps aticker)))
                 (utils/lag 1 (drop-last (vec (ATR-bps aticker))))
                 (utils/lag 2 (drop-last (vec (ATR-bps aticker))))
                 (utils/lag 3 (drop-last (vec (ATR-bps aticker))))
                 (utils/lag 4 (drop-last (vec (ATR-bps aticker))))
;;to predict, lookahead is one here
                 (rest (vec (close-open-bps aticker)))
;;
                 (utils/lag 1 (rest (vec (close-open-bps aticker))))
                 (drop-last (vec (volume-change-bps aticker)))
                 (vec (repeat (count (rest (close-open-bps aticker))) aticker))
                 (drop-last (:dates (ohlcv aticker))))
            :header
            ["consolidated-trend2" "ATR-bps" "ATR-bps.1" "ATR-bps.2" "ATR-bps.3" "ATR-bps.4"  "close-open-bps" "close-open-bps.1" "volume-change-bps" "ticker" "date"]})

         (defn factor-data
           "target format of data is [[] [] [] [] ] for each date "
           [aticker]
           {:data
            (map vector
                 (vec (consolidated-trend2 aticker))
                 (vec (ATR-bps aticker))

;;to predict, lookahead is one here
                 (utils/lookahead 1 (vec (close-open-bps aticker)))
;;

                 (vec (volume-change-bps aticker))
                 (vec (repeat (count (close-open-bps aticker)) aticker))
                 (:dates (ohlcv aticker)))
            :header
            ["consolidated-trend2" "ATR-bps" "close-open-bps" "volume-change-bps" "ticker" "date"]})

         (def small-factor-data
           {:header
            (first (map :header (map factor-data clean-ticnames-small)))

            :data
            (apply concat (map :data (map factor-data clean-ticnames-small)))})

         (def mid-factor-data
           {:header
            (first (map :header (map factor-data clean-ticnames-mid)))

            :data
            (apply concat (map :data (map factor-data clean-ticnames-mid)))})

;;define datagen file


         (defn write-dataset [f_ this-factor-data]
           (let [;first-datapoint? (not (.exists (io/as-file f_)))
                 data (:data this-factor-data)
                 header (:header this-factor-data)]
             (spit f_ (u/coll->string header) :append true)
             (doseq [data-points (map u/coll->string data)]
               (spit f_ data-points :append true))))

         (do (def token (u/random-word))
             (def factor-file (str "/home/puru/data/factors-small-" (str (gensym token)) ".txt"))
             (write-dataset factor-file small-factor-data)
             (pprint factor-file))

         (do (def token (u/random-word))
             (def factor-file (str "/home/puru/data/factors-mid-" (str (gensym token)) ".txt"))
             (write-dataset factor-file mid-factor-data)
             (pprint factor-file))

         (first (:data mid-factor-data))

;; does c trend predict close-open?
;; appears atr -1 is the best single handed predictor
;; spose you use that
;; each date maps to what you will trade
         (def dates (:dates (ohlcv "AAPL")))

         (defn asset-list [factor-data date] (let [tmp-data (filter #(= date (last %)) (:data factor-data))
                                                   trade-these (cond (empty? tmp-data) nil :else
                                                                     (map #(nth % 4) (take 10 (reverse (sort-by #(second %) tmp-data)))))]

                                               trade-these))

;; this the walkforward ticker set we need to test on
         (def trade-these-mid (map vector dates (u/lag 1 (map (partial asset-list mid-factor-data) dates))))

         (spit "/home/puru/Dropbox/data/trade-these-mid.data" (pr-str trade-these-mid))

         (def trade-these-small (map vector dates (u/lag 1 (map (partial asset-list mid-factor-data) dates))))

         (spit "/home/puru/Dropbox/data/trade-these-small.data" (pr-str trade-these-mid)))

;; test work

;(count (filter good-ticker? (map first (filter #(and (= "Major Pharmaceuticals"  (nth % 5) )     (= "Health Care" (nth % 4) )   ) nasdaq-data))))
