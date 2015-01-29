(defproject ardoq-swagger "0.1.0-SNAPSHOT"
  :description "Swagger add-on for Ardoq"
  :url "http://ardoq.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.8"]]
                   :source-paths ["dev"]}
             :uberjar {:aot [ardoq.swagger.server]
                       :main ardoq.swagger.server}}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.3.1"]
                 [ring/ring-core "1.3.2"]
                 [http-kit "2.1.19"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [cheshire "5.4.0"]
                 [clojurewerkz/urly "1.0.0"]
                 [hiccup "1.0.5"]
                 [org.clojure/data.json "0.2.5"]
                 [medley "0.5.5"]])
