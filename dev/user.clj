(ns user
  (:require
   [clojure.java.io :as io]
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [pprint]]
   [clojure.reflect :refer [reflect]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]
   [cheshire.core :refer [parse-string]]
   [superstring.core :as str]
   [ardoq.swagger
    [server :as server]
    [api :as api]
    [util :as util]
    [client :as c]
    [validate :as validate]
    [common :as common]]
   [ardoq.swagger.sync-swagger :as sync-swagger]
   [ardoq.swagger.api :as v1]))

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



(defn oa3 []
  (let [client (c/client {:url "http://piedpiper.localhost:8080"
                          :org "piedpiper"
                          :token "42f5d07007594f61bb7b66548c182b16"})
        spec-text (slurp "test/spec.yaml")
        spec (util/parse-swagger spec-text)]

    (sync-swagger/sync-swagger client spec "swaggertest 5" :openapi-3.x)))


