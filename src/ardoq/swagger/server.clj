(ns ardoq.swagger.server
  (:require [ardoq.swagger.api :refer [swagger-api]]
            [org.httpkit.server :as srv]
            [ring.middleware.params :refer [wrap-params]])
  (:gen-class :main true))

(defn app [system]
  (-> (swagger-api system)
      wrap-params))

(defn start-server [system]
  (srv/run-server (app system) system))

(defn -main []
  (println "Starting server...")
  (srv/run-server (app {:base-url "https://app.ardoq.com"}) {})
  (println "Server started!"))
