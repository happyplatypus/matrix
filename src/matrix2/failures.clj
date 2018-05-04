(ns matrix.failures)

;;some callback issue doesnt work
(defn sim-strategy-agents [agent-coll ticker-coll date-coll]
  (let [agent-states (map #(create-agent-state % ticker-coll date-coll) agent-coll)
        i-state (create-indicator-state ticker-coll date-coll)]
    (doall (map #(for [this-ticker ticker-coll this-date date-coll]
                   (let [tic (keyword  this-ticker)
                         date (keyword (str this-date))
                         agent-listening-to-this-node  @((% tic) date)]
                     (add-callback-agent false i-state agent-listening-to-this-node tic date))) agent-states)) (doall (for [this-ticker ticker-coll this-date date-coll] (update-data-quotes i-state this-ticker this-date)))
    (map #(zipmap (map keyword ticker-coll) (map (partial retrieve-pnls-tmp % date-coll)  ticker-coll)) agent-states)))

(pprint (sim-strategy-agents a-list ["TEVA" "MDRX"] [20171117]))
