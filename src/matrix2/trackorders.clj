(ns matrix.trackorders

  (:require [repltrader.gateway :as g]
            [matrix.tickers :as tickers]
            [matrix.timekeeper :as timekeeper]

            [matrix.indicators :as indicators]
            [matrix.pnl :as pnl]
            [incanter [core :refer [$]
                       :as incanter$]
             [core :as incanter]
             [stats :as stats]
             [io :as io2]
             [charts :as charts]
             [datasets :as dataset]] [clojure.term.colors :refer :all])
  (:use clojure.pprint)
  (:gen-class))

;; on tickers/ticlist-big as sometimes older positions have to be reset on clean
(def portfolio-state (zipmap (map keyword tickers/huge-tickers) (repeatedly (count tickers/huge-tickers) #(atom "Disengaged"))))

;; when this is true, we are waitning for order response
(def high-impedance (zipmap (map keyword tickers/huge-tickers) (repeatedly (count tickers/huge-tickers) #(atom false))))

;(println high-impedance)

(defn reset-portfolio-state [tic]
  (reset! ((keyword tic) portfolio-state) "Disengaged"))

(defn reset-impedance-state-false [tic]
  (reset! ((keyword tic) high-impedance) false))

;(comment
(def order-fills (zipmap (map keyword tickers/huge-tickers) (repeatedly (count tickers/huge-tickers) #(atom 0))))

;; plus or minus, actual size filled
(def order-inventory (zipmap (map keyword tickers/huge-tickers) (repeatedly (count tickers/huge-tickers) #(atom 0))))
;; the above the actual fill quantities


(def order-fill-time (zipmap (map keyword tickers/huge-tickers) (repeatedly (count tickers/huge-tickers) #(atom 0))))

(def order-ids (atom {}))
(def exec-ids (atom {}))

;(swap! order-ids assoc (:permanent-id (:order msg)) [(:action (:order msg))] )
;(swap! order-ids assoc 123 "sell")


;(identity @order-ids)

(comment handle-exec-update [{:keys [type contract] :as msg}]
         (when  (and (= type :open-order) (= :filled (:status (:order-state msg))) (not (contains? (set (keys @order-ids))  (:order-id msg))))
           (println "exec update" msg)

           (cond (= :buy (:action (:order msg))) (swap! ((keyword (:local-symbol (:contract msg))) order-inventory) +  (:quantity (:order msg)))
                 :else (swap! ((keyword (:local-symbol (:contract msg))) order-inventory) - (:quantity (:order msg))))

           (swap! order-ids assoc (:order-id msg) (:local-symbol (:contract msg)))))

;;; check how many times this updates for a trade
(comment handle-stat-update [{:keys [type contract] :as msg}]
         (when  (and (= type :order-status) (zero? (:remaining msg)))
           (println "stat update" msg)
           (let [tic (@order-ids  (:order-id msg))]
             (reset! ((keyword tic) order-fills)    (:last-fill-price msg)))))

(defn handle-exec-update
  "exec details message contains all information tied to the order "
  [{:keys [type contract] :as msg}]
  (when

   (and
           ;; a type exec details message
    (= type :execution-details)

           ;; new order exec report, its not there in my order-ids
    (not (contains? (set (keys @order-ids))  (:order-id (:value msg)))))

   ;; inventory management
    (= :buy (:side (:value msg)))
    (cond (= :buy (:side (:value msg)))  (do
                                           (swap! ((keyword (:local-symbol (:contract msg))) order-inventory) +  (:shares (:value msg)))
                                        ;(if (zero? @((keyword (:local-symbol (:contract msg))) order-inventory) ) (reset-portfolio-state (:local-symbol (:contract msg))))
                                           (reset! ((keyword (:local-symbol (:contract msg))) order-fill-time) @timekeeper/global-time-now)
                                           (reset-impedance-state-false (:local-symbol (:contract msg)))

                                           (pprint (on-white (green (str "increased inventory from IB, tic pnl  " (:local-symbol (:contract msg)) " "  @pnl/pnl)))))
          (= :sell (:side (:value msg)))

          (do (swap! ((keyword (:local-symbol (:contract msg))) order-inventory) -  (:shares (:value msg)))
              (reset! ((keyword (:local-symbol (:contract msg))) order-fill-time) @timekeeper/global-time-now)
              (reset-impedance-state-false (:local-symbol (:contract msg)))

              (pprint (on-white (green (str "decreased inventory from IB, tic pnl  " (:local-symbol (:contract msg)) " " @pnl/pnl)))));(if (zero? @((keyword (:local-symbol (:contract msg))) order-inventory) ) (reset-portfolio-state (:local-symbol (:contract msg))) )
);;update the order and exec id database, order is key and value is ticker
    (swap! order-ids assoc (:order-id (:value msg)) (:local-symbol (:contract msg)))
    (swap! exec-ids assoc (:execution-id (:value msg)) (:local-symbol (:contract msg)))

                                        ;if i got a fill that took inventory to a non zero level, update the price that fires an inventory neutralizer maybe

    ;; right now if pending cancel etc, it does nothing
    (let [tic (@order-ids  (:order-id (:value msg)))

          qty @((keyword (:local-symbol (:contract msg))) order-inventory)]

      (when-not (zero? qty)
        (reset! ((keyword tic) order-fills)    (:price (:value msg)))))))

;(def orderidQueue (zipmap tickers/ticlist (repeatedly (count tickers/ticlist) #(atom 0) )))
;(def targetpositionQueue (zipmap tickers/ticlist (repeatedly (count tickers/ticlist) #(atom 0) )))
;(def targetsideQueue (zipmap tickers/ticlist (repeatedly (count tickers/ticlist) #(atom "flat") )))
;(def realizedpositionQueue (zipmap tickers/ticlist (repeatedly (count tickers/ticlist) #(atom 0) )))


(comment update-latest-order-id [msg]
         "otherwise I lose track of which order is latest, i keep only latest order"
         (when (= (keyword "open-order") (:type msg))
           (reset! (orderidQueue (:symbol (:contract msg)))  (:permanent-id (:order msg)))

           (if (= (keyword "buy") (:action (:order msg)))

             (do
               (reset! (targetpositionQueue (:symbol (:contract msg))) (:quantity (:order msg)))
               (reset! (targetsideQueue (:symbol (:contract msg))) "buy"));(comment(reset! (targetpositionQueue (:symbol (:contract msg))) (- (:quantity (:order msg)))))
      ;(comment (reset! (targetsideQueue (:symbol (:contract msg))) "sell" ))
)))

(comment (defn update-latest-fill [msg]
           (when (= (keyword "order-status") (:type msg))
             (if (targetsideQueue tic))
             (reset! (realizedpositionQueue (:symbol (:contract msg))) (:filled msg)))))

(do
  (g/subscribe handle-exec-update)
   ;(g/subscribe update-latest-order-id)
)
;)


;; this is order, first open order, then exec details
;; when order is cancelled you dont get exec report
(def msg {:type :open-order, :order-id 305, :contract {:contract-id 5911, :symbol "BIG", :exchange "SMART", :local-symbol "BIG", :currency "USD", :type :equity, :include-expired? false, :put-call-right :unknown}, :order {:time-in-force :immediate-or-cancel, :all-or-none? false, :outside-regular-trading-hours? false, :client-id 100, :block-order? false, :limit-price 0.0, :stop-price 0.0, :transmit? true, :type :market, :order-id 305, :hidden? false, :action :buy, :quantity 100, :discretionary-amount 0.0, :sweep-to-fill? false, :permanent-id 1148577392}, :order-state {:status :filled, :initial-margin "1.7976931348623157E308", :maintenance-margin "1.7976931348623157E308", :equity-with-loan "1.7976931348623157E308", :commission 1.7976931348623157E308, :minimum-commission 1.7976931348623157E308, :maximum-commission 1.7976931348623157E308}})

(def msg {:type :execution-details, :request-id -1, :contract {:contract-id 5911, :symbol "BIG", :exchange "ISLAND", :local-symbol "BIG", :currency "USD", :type :equity, :include-expired? false}, :value {:client-id 100, :account-code "DU242353", :cummulative-quantity 100, :time "20170825  13:34:06", :average-price 48.73, :order-id 305, :execution-id "00018037.59a04efc.01.01", :side :buy, :exchange "ISLAND", :price 48.73, :shares 100, :permanent-id 1148577392, :liquidate-last 0}})

(def msg {:last-fill-price 48.73, :average-fill-price 48.73, :client-id 100, :remaining 0, :why-held nil, :type :order-status, :status :filled, :order-id 305, :parent-id 0, :filled 100, :permanent-id 1148577392})

(def msg {:type :open-order, :order-id 305, :contract {:contract-id 5911, :symbol "BIG", :exchange "SMART", :local-symbol "BIG", :currency "USD", :type :equity, :include-expired? false, :put-call-right :unknown}, :order {:time-in-force :immediate-or-cancel, :all-or-none? false, :outside-regular-trading-hours? false, :client-id 100, :block-order? false, :limit-price 0.0, :stop-price 0.0, :transmit? true, :type :market, :order-id 305, :hidden? false, :action :buy, :quantity 100, :discretionary-amount 0.0, :sweep-to-fill? false, :permanent-id 1148577392}, :order-state {:status :filled, :initial-margin "1.7976931348623157E308", :maintenance-margin "1.7976931348623157E308", :equity-with-loan "1.7976931348623157E308", :commission 1.0, :minimum-commission 1.7976931348623157E308, :maximum-commission 1.7976931348623157E308, :commission-currency "USD"}})

(def msg {:last-fill-price 48.73, :average-fill-price 48.73, :client-id 100, :remaining 0, :why-held nil, :type :order-status, :status :filled, :order-id 305, :parent-id 0, :filled 100, :permanent-id 1148577392})

(def msg {:type :commission-report, :report {:commission 1.0, :currency "USD", :execution-id "00018037.59a04efc.01.01", :realized-profit-loss 1.7976931348623157E308, :yield 1.7976931348623157E308, :yield-redemption-date 0}})

(def msg {:type :open-order, :order-id 305, :contract {:contract-id 5911, :symbol "BIG", :exchange "SMART", :local-symbol "BIG", :currency "USD", :type :equity, :include-expired? false, :put-call-right :unknown}, :order {:time-in-force :immediate-or-cancel, :all-or-none? false, :outside-regular-trading-hours? false, :client-id 100, :block-order? false, :limit-price 0.0, :stop-price 0.0, :transmit? true, :type :market, :order-id 305, :hidden? false, :action :buy, :quantity 100, :discretionary-amount 0.0, :sweep-to-fill? false, :permanent-id 1148577392}, :order-state {:status :filled, :initial-margin "1.7976931348623157E308", :maintenance-margin "1.7976931348623157E308", :equity-with-loan "1.7976931348623157E308", :commission 1.0, :minimum-commission 1.7976931348623157E308, :maximum-commission 1.7976931348623157E308, :commission-currency "USD"}})

(def msg {:last-fill-price 48.73, :average-fill-price 48.73, :client-id 100, :remaining 0, :why-held nil, :type :order-status, :status :filled, :order-id 305, :parent-id 0, :filled 100, :permanent-id 1148577392})
(:symbol (:contract msg))

(def msg2
  {:type :execution-details, :request-id -1, :contract {:include-expired? false, :type :equity,
                                                        :currency "USD", :local-symbol "Y", :exchange "NYSE", :symbol "Y", :contract-id 13999}, :value {:client-id 100, :account-code "DU242353",
                                                                                                                                                        :cummulative-quantity 100, :time "20151109  10:53:40", :average-price 501.78, :order-id 463, :execution-id "00018037.56494702.01.01",
                                                                                                                                                        :side :sell, :exchange "NYSE", :price 501.78, :shares 100, :permanent-id 215958916, :liquidate-last 0}})

;(reset! (orderidQueue (:symbol (:contract msg)))  (:permanent-id (:order msg)))
;(reset! (orderidQueue (:symbol (:contract msg)))  (:permanent-id (:order msg)) )

(:action (:order msg))
