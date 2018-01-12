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
                 [clj-http "1.1.2" :exclusions [com.fasterxml.jackson.core/jackson-databind]]
                 [ring/ring-core "1.3.2"]
                 [http-kit "2.1.19"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [cheshire "5.4.0"]
                 [clojurewerkz/urly "1.0.0"]
                 [hiccup "1.0.5"]
                 [superstring "2.1.0"]
                 [io.forward/yaml "1.0.6"]
                 [com.github.fge/json-schema-validator "2.2.6"]
                 [com.google.guava/guava "18.0"]
                 [org.clojure/data.json "0.2.5"]
                 [medley "0.5.5"]])
