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
   [ardoq.client :as c]
   [ardoq.swagger
    [server :as server]
    [api :as api]
    [util :as util]
    [validate :as validate]
    [common :as common]
    [sync-overview-workspace :as sync-overview-workspace]]

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



(defn o []
  (try
    (let [client (c/client {:url   "http://localhost:8080"
                            :org   "piedpiper"
                            :token "42f5d07007594f61bb7b66548c182b16"})
          spec-text (slurp "test/spec.yaml")
          spec (util/parse-swagger spec-text)]

      (prn "importing" client)
      (sync-swagger/sync-swagger client spec "OpenAPI spec.yaml 2e" :openapi-3.x))
    (catch Exception e (.printStackTrace e))))



(defn sw []
  (try
    (let [client (c/client {:url "http://localhost:8080"
                            :org "piedpiper"
                            :token "42f5d07007594f61bb7b66548c182b16"})
          spec-text (slurp "dev-resources/dnbswag.yaml")
          spec (util/parse-swagger spec-text)]

     (sync-swagger/sync-swagger client spec "swagger 2 test 3" :swagger-2.x))
    (catch Exception e (.printStackTrace e)))
  )


(defn p []
  (try
    (let [client (c/client {:url "http://localhost:8080"
                            :org "piedpiper"
                            :token "42f5d07007594f61bb7b66548c182b16"})
          spec-text (slurp "http://petstore.swagger.io/v2/swagger.json")
          spec (util/parse-swagger spec-text)]
      #_(clojure.pprint/pprint spec)
      (clojure.pprint/pprint
        (c/find-components-by-name client "5a97fa7bd6018000060a8468" "Swagger Petstore"))

      #_(sync-swagger/sync-swagger client spec "petstore.json" :swagger-2.x))
    (catch Exception e (.printStackTrace e))))


(defn e []
  (try
    (let [client (c/client {:url "http://localhost:8080"
                            :org "piedpiper"
                            :token "42f5d07007594f61bb7b66548c182b16"})]
      (clojure.pprint/pprint
        (sync-overview-workspace/ensure-entry-in-overview-ws client {:name "Swagger spec"}
                                                             {:overview-workspace "App Portfolio"
                                                              :overview-component-type "Application"
                                                              :overvice-reference-type "Exists"}))

      )
    (catch Exception e (.printStackTrace e))))