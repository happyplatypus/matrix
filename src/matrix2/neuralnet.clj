(ns matrix.neuralnet

  (:require
   [matrix.utils :as utils]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clj-http.client :as client]
   [clj-time.format :as tf]
   [clj-time.core :as tt]
   [incanter.interpolation :refer :all]
   [matrix.train :as train]
   [cortex.nn.execute :as execute]
   [cortex.nn.layers :as layers]
   [cortex.nn.network :as network]

   [incanter [core :refer [$]
              :as incanter$]
    [core :as incanter]
    [stats :as stats]
    [io :as io2]
    [charts :as charts]
    [datasets :as dataset]
    ;[interpolation :as interpolation]
])

  (:use clojure.pprint)

  (:gen-class))

(def trade-dataset (io2/read-dataset "/home/puru/Dropbox/data/regdata.csv" :header false :delim \space))
(incanter$/$ :col0 trade-dataset)

(incanter$/$ [:col0 :col1]  trade-dataset)

(def trade-dataset (str/split (slurp "/home/puru/Dropbox/data/regdata.csv") #"\n"))

(read-string (first (str/split (first trade-dataset) #" ")))
(map read-string (rest (str/split (first trade-dataset) #" ")))

(defn create-dataset [trade-dataset-line] {:y [(read-string (first (str/split trade-dataset-line #" ")))] :x (vec (map read-string (rest (str/split trade-dataset-line #" "))))})
(def xor-dataset (map create-dataset trade-dataset))
(first xor-dataset)
(def xor-dataset
  [{:x [0.0 0.0] :y [0.0]}
   {:x [0.0 1.0] :y [1.0]}
   {:x [1.0 0.0] :y [1.0]}
   {:x [1.0 1.0] :y [0.0]}])

;; Definition of the neural network
(def nn
  (network/linear-network
   [(layers/input 8 1 1 :id :x) ;; input :x 2*1 dimensions
    (layers/linear->tanh 10)
    (layers/linear->tanh 15)
    (layers/linear->tanh 10)
    (layers/linear 1 :id :y)]))

(defn train-xor []
  (let [trained (train/train-n nn xor-dataset xor-dataset
                               :batch-size 4
                               :epoch-count 100
                               :simple-loss-print? true)]
    (println "\nXOR results before training:")
    (clojure.pprint/pprint (execute/run nn xor-dataset))
    (println "\nXOR results after training:")
    (clojure.pprint/pprint (execute/run trained xor-dataset))))

(train-xor)
