(defproject common "0.1.3"
  :description "Liberator compojure-api proof of concept"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :global-vars  {*warn-on-reflection* false}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.1" :exclusions [[org.clojure/clojure]]]
                 [compojure "1.3.1"]
                 [liberator "0.12.2"]
                 [metosin/compojure-api "0.21.0"]
                 [io.clojure/liberator-transit "0.3.0"]
                 [ring/ring-core "1.3.2"]
                 [cheshire "5.4.0"]
                 [com.cognitect/transit-clj "0.8.259"]
                 ]
            :aot [clojure.tools.logging.impl]
    :profiles  {:dev {:dependencies [[ring/ring-mock "0.2.0"]]}})
