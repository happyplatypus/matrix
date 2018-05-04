(ns matrix.pnl
  (:require [repltrader.gateway :as g]
            [clojure.term.colors :refer :all])
  ;(:require [matrix.trackorders :as t])


; white, cyan, magenta, blue, yellow, green, red, grey, on-white,
;on-cyan, on-magenta, on-blue, on-yellow, on-green, on-red, on-grey,
;concealed, reverse-color, blink, underline, dark, bold
)

(defonce pnl (atom 0))

(defn handle-pnl-update [{:keys [type contract] :as msg}]
  (when  (and (= type :commission-report) (> 1000000 (:realized-profit-loss (:report msg))))
    ;(println msg)
    (println
     (on-blue (green (str "*** IB Pnl Report ****    " (:realized-profit-loss (:report msg))))))

    (swap! pnl + (:realized-profit-loss (:report msg)))

    (println (on-blue (red (str "Running PNL : " @pnl))))))

(do
  (g/subscribe handle-pnl-update))
