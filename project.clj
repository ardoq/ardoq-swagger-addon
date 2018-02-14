(defproject ardoq-swagger-addon "1.0.5"
  :description "Swagger add-on for Ardoq"
  :url "http://ardoq.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.10"]]
                   :source-paths ["dev"]
                   :plugins        [[test2junit "1.1.2"
                                     :exclusions [org.clojure/clojure]]
                                    [jonase/eastwood "0.2.5"
                                     :exclusions [org.clojure/clojure]]]}

             :uberjar {:aot [ardoq.swagger.server]
                       :main ardoq.swagger.server}}
  :plugins [[lein-ancient "0.6.15"]
            [com.ebaxt.lein-ver "1.2.0"]]
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [compojure "1.6.0"]
                 [clj-http "3.7.0" :exclusions [com.fasterxml.jackson.core/jackson-databind]]
                 [ring/ring-core "1.6.3"]
                 [http-kit "2.2.0"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [cheshire "5.8.0"]
                 [clojurewerkz/urly "1.0.0"]
                 [hiccup "1.0.5"]
                 [superstring "2.1.0"]
                 [io.forward/yaml "1.0.6"]
                 [com.github.fge/json-schema-validator "2.2.6"]
                 [com.google.guava/guava "23.0"]
                 [org.clojure/data.json "0.2.6"]
                 [medley "1.0.0"]])
