(defproject matrix2 "0.1.0-SNAPSHOT"
  :description "To generate summary stats of indicors on stock for a
day, for example, what was the average bps returns on target stock in the past 5 days - what does that mean for tomorrow's trading? Contains some strategy ideas based on entry and exit. The key concept is to generate a 3-D cube of
assets on y axis, agents(strategies) on x and dates on z. Note that when z is collapsed with summaries, we get a asset agent matrix, which can be used to decide what pairs to run tomorrow.
Needs a subscription to active tick
http://www.activetick.com/activetick/contents/
"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ["-Xms2048m" "-Xmx4096m"]
  :dependencies [
                 [clj-http "3.3.0"]
                 [enlive "1.1.1"]
                 [org.clojure/clojure "1.8.0"]
                 [com.ib/jtsclient "9.68"]
                 [incanter "1.5.6"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [clj-logging-config "1.9.10"]
                 [me.raynes/conch "0.8.0"]
                 [clojure-watch "LATEST"]
		 [com.draines/postal "1.11.3"]
		 [me.raynes/fs "1.4.6"]
                 [http.async.client "0.5.2"]
                 [org.slf4j/slf4j-simple "1.7.2"]
                 [clojure-csv/clojure-csv "2.0.1"]
                 [http-kit "2.1.18"]
                 [org.slf4j/slf4j-simple "1.7.2"]
                 [prismofeverything/ring-buffer "1.0.0"]
                 [org.clojure/data.json "0.2.6"]
                 [semantic-csv "0.2.1-alpha1"]
                 [tide "0.2.0-20170806.131424-3" :exclusions [org.apache.commons/commons-math3]]
                 [hswick/jutsu "0.1.2"]


;                 [huri "0.7.0-SNAPSHOT"]
                 ;;ib gateway requires

                 [org.clojure/tools.logging "0.2.3"]
                 [clj-logging-config "1.9.10"]
                 [clojure-watch "LATEST"]
                 [net.mikera/vectorz-clj "0.43.0"]
                 [me.raynes/conch "0.8.0"]
                 [clj-highcharts "0.1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [thinktopic/cortex "0.9.22"]

                 ;; for prediction with trees
                 [org.clojure/math.combinatorics "0.1.4"]

                 [clojure-csv "2.0.2"]
                 [clj-time "0.14.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [clojure-term-colors "0.1.0-SNAPSHOT"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [bigml/histogram "4.1.3"]
                 ;[com.yetanalytics/dl4clj "0.1.0-alpha"]

                 ]

  :profiles {:dev {:dependencies [[org.clojure/test.check "0.10.0-alpha2"]
                                  [walmartlabs/datascope "0.1.1"]
                                  [pjstadig/humane-test-output "0.8.3"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]
                   :plugins [[lein-cloverage "1.0.9"]

                               [lein2-eclipse "2.0.0"]
           [lein-codox "0.9.4"]
           [lein-gorilla "0.4.0"]
           [lein-marginalia "0.9.1"]


           [lein-bin "0.3.4"]



                             ]


                   }}




  :plugins [
            [lein2-eclipse "2.0.0"]
           [lein-codox "0.9.4"]
           [lein-gorilla "0.4.0"]
           [lein-marginalia "0.9.1"]
	   [lein-cljfmt "0.5.7" ]

           [lein-bin "0.3.4"]

           ]

  :bin {:name "tradeinit"
        ;:name "sim"
        ;:name "datagen"
          :bin-path "~/.local/bin"
        ;:bootclasspath true
        }

  ;:main gorilla-test.core
  :main matrix.data
;  :main cointegrate.backtest
;:aot :all
;:profiles {:uberjar {:aot :all}}
  )
