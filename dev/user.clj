(ns user
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]
   [cheshire.core :refer [parse-string]]
   [ardoq.swagger 
    [server :as server]
    [swagger :as swagger]
    [api :as api]
    [client :as c]
    [validate :as validate]
    [common :as common]]
   [ardoq.swagger.swagger-v2
    :refer :all]
   [ardoq.swagger.api
    :refer :all]))

(def system
  "A Var containing an object representing the application under
  development."
  (atom {}))

(defn start
  "Starts the system running, updates the Var #'system."
  []
  (reset! system {:server (server/start-server {:config {:base-url "http://localhost:8080"}
                                                :port 4000})}))

(defn stop
  "Stops the system if it is currently running, updates the Var
  #'system."
  []
  (when-let [s (:server @system)]
    (s)))

(defn go
  "Initializes and starts the system running."
  []
  (start)
  :ready)

(defn reset
  "Stops the system, reloads modified source files, and restarts it."
  []
  (stop)
  (refresh :after 'user/go))

(defn validate-all []
  (let [files (drop 1 (file-seq (io/file "")))]
    (doseq [f files]
      (doall
       (import-swagger2 (c/client {:url "http://dockerhost"
                                   :org "demo"
                                   :token ""})
                        (parse-string (slurp f) true) (.getName f))))))
