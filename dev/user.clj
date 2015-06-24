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
    [client :as c]]
   [ardoq.swagger.swagger-v2
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

(defn example []
  (-> (io/resource "example.json")
      (io/reader)
      slurp
      (parse-string keyword)))

;;This is a test cleint to ease implementation. Delete upon completion
(def client (c/client {:url "http://127.0.0.1:8080"
                       :token "9b2a9517e5c540a791f9db2468866a4f"
                       :org "ardoq"}))
