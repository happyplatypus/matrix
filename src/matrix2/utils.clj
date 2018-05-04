(ns matrix.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [incanter.zoo :as zoo]
   [clj-time.format :as tf]
   [clj-time.core :as tt]
   [clj-time.local :as l]
   [clojure.java.io :as io]
   [clojure.string :as str]

   [incanter [core :refer [$]
              :as incanter$]
    [core :as incanter]
    [stats :as stats]
    [io :as io2]
    [charts :as charts]
    [datasets :as dataset]]
   [matrix.utils :as utils])
  (:gen-class))

(defn abs [x] (if (> x 0) (identity x) (- x)))

(defn formatlocal [n offset]
  (let [nlocal (tt/to-time-zone n (tt/time-zone-for-offset offset))]
    (tf/unparse (tf/formatter-local "yyyy-MM-dd hh:mm:ss aa")
                nlocal)))

(defn currentTime []
  (formatlocal (tt/now) -4))

(defn csum [s]
  (take (count s) (reductions + s)))

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn round
  "Round a double to the given precision (number of significant digits)"
  [d]
  (round2 2 d))

(defn sign [x]
  (cond (< x 0)   (- 1)
        (> x 0)   (identity 1)
        :else     (identity 0)))

(defn- read-one
  [r]
  (try
    (read r)
    (catch java.lang.RuntimeException e
      (if (= "EOF while reading" (.getMessage e))
        ::EOF
        (throw e)))))

(defn read-seq-from-file
  "Reads a sequence of top-level objects in file at path."
  [path]
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader path))]
    (binding [*read-eval* false]
      (doall (take-while #(not= ::EOF %) (repeatedly #(read-one r)))))))

(defn log-diff [x]
  (let [p (map incanter/log x) out (map - (rest p) (drop-last p))] (identity out)))

(defn diff_ [x] (map - (rest x) (drop-last x)))

(defn sign_ [x]
  (if (zero? x) (identity 0) (/ x (stats/scalar-abs x))))

(defn correlation_ [x y]
  (let [L (min
           (count x)
           (count y))]
    (stats/correlation (take L x) (take L y))))

(defn signalize [x]
  (cond (<  (stats/scalar-abs x) 0.95)  (identity 0)
        (> x 0.95) (identity 1)
        (< x (- 0.95)) (- 1)))

;(defn sharpe[x] (/ (stats/mean x) (stats/sd x) ))


(defn sharpe [x]
  (let [m  (stats/mean x)
        s (stats/sd x)]
    (if (zero? s) (identity 0) (round (/ m s)))))

(defn pnl-sharpe
  "sharpe pnl-avg N min max"
  [x]
  (let [m  (stats/mean x)
        s (stats/sd x)]
    (if (zero? s) (identity [0 0 0 0 0])
        [(round (/ m s))  m (count x) (apply min x) (apply max x)])))

;(pnl-sharpe [1 2 3 4 5])


(defn ema [HL values]
  (cond  (< (count (remove nil? values))  (+ 1 HL)) nil
         :else

         (last (map float (reductions (fn [running v]
                                        (let [f (/ 2 (+ 1 HL))
                                              one-minus-F (- 1 f)] ;naming intermediate results can help with the readability of non-associative operators.
                                          (+ (* f v)
                                             (* one-minus-F running))))
                                      values)))))

(defn ema-all
  "returns entire seq"
  [HL values]
  (cond  (< (count (remove nil? values))  (+ 1 HL)) nil
         :else

         (map float (reductions (fn [running v]
                                  (let [f (/ 2 (+ 1 HL))
                                        one-minus-F (- 1 f)] ;naming intermediate results can help with the readability of non-associative operators.
                                    (+ (* f v)
                                       (* one-minus-F running))))
                                values))))

;(reverse (ema-all 10 (reverse '(1 2 3 4 5 6 7 8 9 10 11 ))))

(defn lPlot2 [one two]
;(incanter/view (charts/line-chart (range 0 (count x)) x))


  (doto (charts/scatter-plot (range (count one)) one)
    (charts/add-lines (range 0 (count two)) two)
    incanter/view))

(defn lPlot3 [one two three]
;(incanter/view (charts/line-chart (range 0 (count x)) x))


  (doto (charts/scatter-plot (range (count one)) one)
    (charts/add-lines (range 0 (count two)) two)
    (charts/add-lines (range 0 (count three)) three)

    incanter/view))

(defn lPlot [x]
  (incanter/view (charts/line-chart (range 0 (count x)) x)))

(defn msec
  "msecs since 9:30"
  [tstamp]
  (let [hours

        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 8)
            (nth (str/split tstamp #"") 9)))

        minutes
        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 10)
            (nth (str/split tstamp #"") 11)))

        seconds
        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 12)
            (nth (str/split tstamp #"") 13)))

        mseconds

        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 14)
            (nth (str/split tstamp #"") 15)
            (nth (str/split tstamp #"") 16)))]
  ;(identity mseconds)
    (int (+ mseconds (int (* 1000 (reduce + [(* 3600 (- hours 9)) (* 60 minutes) seconds])))))))

(defn tradingTime
  "actual time of the time stamp in AT"
  [tstamp]
  (let [hours

        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 8)
            (nth (str/split tstamp #"") 9)))

        minutes
        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 10)
            (nth (str/split tstamp #"") 11)))

        seconds
        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 12)
            (nth (str/split tstamp #"") 13)))

        mseconds

        (. Integer parseInt
           (str
            (nth (str/split tstamp #"") 14)
            (nth (str/split tstamp #"") 15)
            (nth (str/split tstamp #"") 16)))]
    (str/join " " [hours minutes seconds])
  ;(int (+ mseconds (int (* 1000 (reduce + [(* 3600 (- hours 9)) (* 60 minutes) seconds]) ))))
))

(defn zscore [coll]
  "divide series by its own sd"
  (if (nil? coll) (identity nil) (let  [m (stats/mean coll)
                                        s (stats/sd coll)
                                        s-clean (cond (zero? s) (identity nil) :else (identity s))
                                        normalized (map #(/ (- % m) s-clean) coll)] (map (partial round2 2) normalized))))

;(conj [1 2] '())

;(println (filter #(< % (- 3)) (zscore rets)))
(use 'clojure.data)

(defn winsorize [K coll]
  "winsorize at K sigma"
  (if (nil? coll) (identity nil)

      (if (nil? (zscore coll))  (identity nil)
          (let [z (zscore coll)
                s (stats/sd coll)
                m (stats/mean coll)

                original  (map vector (range (count z)) z)

                bigs (filter #(> (abs (second %)) K) original)
                result (cond (empty? bigs) (identity coll)
                             :else (let [bigs-complement (filter #(not (> (abs (second %)) K)) original)

          ;;replace second element with 3 sigma
                                         bigs-mod (if (not (empty? bigs))  (map #(assoc % 1 (* 3 (sign (second %)))) bigs))
                                         new-zscore (sort-by first (remove nil? (concat bigs-mod bigs-complement)))
                                         reconstructed (map (partial round2 2)  (map #(+ (* (second %)  s) m) new-zscore))] (identity reconstructed)))] (identity result)))))

;;test for winsorize
(comment (def rets (let [aticker "BBBY"
                         tech (ohlcv aticker)
                         p (:c tech)
                         rets (cond (> (count p) 1) (map return_bps (rest p)  (drop-last p))
                                    :else (vector Double/NaN))]
                     (if (Double/isNaN (first rets)) (identity 0) (let [adj-price (u/csum rets)]
                                                                    (identity rets)))))

         (println (winsorize 2 rets))
         (u/lPlot2 (u/csum rets)  (u/csum (winsorize 2 rets)))) (defn normalize [coll]
                                                                  "divide series by its own sd"
                                                                  (if (nil? coll) (identity nil) (let  [s (stats/sd coll)
                                                                                                        s-clean (cond (zero? s) (identity Double/NaN) :else (identity s))
                                                                                                        normalized (map #(/ % s) coll)] (identity normalized))))

(defn return_bps [x y]
  (if (or (nil? x) (nil? y) (zero? y)) (identity 0)
      (round2 2 (* 10000 (/ (- x y) y)))))

(defn return-bps
  "y is reference"
  [x y]
  (if (or (nil? x) (nil? y) (zero? y)) (identity 0)
      (round2 2 (* 10000 (/ (- x y) y)))))

(defn return-bps-col [col]

  (cond (not= (count col) 2)  nil :else
        (let [x (second col)
              y (first col)]
          (if (or (nil? x) (nil? y) (zero? y)) (identity 0)
              (round2 2 (* 10000 (/ (- x y) y)))))))

(defn sd
  "more forgiving sd"
  [coll]

  (round2 2 (stats/sd coll)))

(defn sanitize [word] (apply str (filter (fn [x] (Character/isLetter x)) word)))

(defn random-word [] (sanitize (rand-nth (str/split (slurp "/usr/share/dict/words") #"\n"))))

;(repeatedly 100 #(random-word ))

(defn coll->string
  "write coll to string with new line"
  [coll] (str (str/join "," coll) "\n"))

(defn winsorize-returns
  "if x in bps cutoff at 10000 bps, 100% return"
  [x]
  (cond  (> (incanter/abs x) 10000)
         (* (sign x) 10000) :else x))

(defn lag
  "if x [ 1 2 3] inc in time, lag 1 x is [NaN 1 2]"
  [n x]
  (let [N (apply min [n (count x)])
        pad (repeat N Double/NaN)]
    (concat pad (drop-last n x))))
;(lag 1 [1 2 3])

(defn lookahead
  "if x [ 1 2 3] inc in time, lookahead 1 x is [2 3 NaN]"
  [n x]
  (let [N (apply min [n (count x)])
        pad (repeat N Double/NaN)]
    (concat (drop n x) pad)))

;(lookahead 10 [1 2 3 4 5 6])

(defn yyyymmdd->epoch [date] (.getMillis (tt/date-time (. Integer parseInt (subs (str date) 0 4))
                                                       (. Integer parseInt (subs (str date) 4 6))
                                                       (. Integer parseInt (subs (str date) 6 8)))))

(defn view-indicator [indicator-map]
  (let [dates (map yyyymmdd->epoch (:dates indicator-map))
        data (:data indicator-map)]
    (incanter/view (charts/time-series-plot dates data
                                            :x-label "Year"))))

(defn coll->string
  "write coll to string with new line"
  [coll] (str (str/join "," coll) "\n"))

(defn coll->string2
  "write coll to string with new line"
  [coll] (str (str/join " " coll) "\n"))

(def HOME (. System getProperty "user.home"))

(defn notnan? [x]

  (cond (string? x) true :else

        (not
         (Double/isNaN x))))

(defn zscore2 [x coll]

  ;; if anything in x coll is nan then nan
  ;; if coll sd is 0 then nan
  ;; limit at +3 and -3
  (let [m (stats/mean coll)
        s (stats/sd coll)

        out (cond (zero? s) 0.0 :else (round2 2 (/ (- x m) s)))]
    out))

(defn drawdown-at-pnl [pnl]
  (cond
    (<= pnl 40.0) Double/NaN  ;; dont take profit here yet
    ;;changed my mind, need to cut loss aggreessively
 ;(<= pnl 50) 50  ;; cut your losers at 50 bucks

    (or (nil? pnl) (Double/isNaN pnl)) Double/NaN

    ;(and (> pnl 10.0) (<= pnl 20.0) )  5.0
    ;(and (> pnl 20.0) (<= pnl 40.0) )  10.0
    (and (> pnl 40.0) (<= pnl 50.0))  15.0
    (and (> pnl 50.0) (<= pnl 100.0))  25.0
    (and (> pnl 100.0) (<= pnl 150.0))  25.0
    (and (> pnl 150.0) (<= pnl 200.0))  25.0
    (and (> pnl 250.0) (<= pnl 300.0))  35.0
    (and (> pnl 350.0) (<= pnl 400.0))  35.0
    (and (> pnl 450.0) (<= pnl 500.0))  45.0
    :else 15.0))
