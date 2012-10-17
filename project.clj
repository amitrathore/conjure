(def common-deps '[])

(defproject org.clojars.runa/conjure "1.0.3"
  :description "Conjure mocking and stubbing"
  :dependencies ~(cons '[org.clojure/clojure "1.3.0"]
                   common-deps)
  :dev-dependencies [[jonase/kibit "0.0.3"]
                     [jonase/eastwood "0.0.2"]
                     [lein-multi "1.1.0"]]
  :plugins [[lein-swank "1.4.4"]
            [lein-difftest "1.3.8"]]
  :multi-deps {"1.2.0" [[org.clojure/clojure "1.2.0"]]
               "1.2.1" [[org.clojure/clojure "1.2.1"]]
               "1.3.0" [[org.clojure/clojure "1.3.0"]]
               "1.4.0" [[org.clojure/clojure "1.4.0"]]
               "1.5.0" [[org.clojure/clojure "1.5.0-alpha3"]]
               :all ~common-deps})


(def common-deps '[[org.clojure/tools.logging "0.2.3"]])

