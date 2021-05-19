(ns ardoq.swagger.server
  (:require [ardoq.swagger.api :refer [swagger-api]]
            [org.httpkit.server :as srv]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]])
  (:gen-class :main true))

(defn app [system]
  (-> system
      swagger-api
      wrap-cookies
      wrap-params
      wrap-multipart-params))

(defn start-server [system]
  (srv/run-server (app system) system))

(defn -main []
  (if-let [base-url (or (.. System (getProperties) (get "API_BASE_URL"))
                        (System/getenv "API_BASE_URL"))]
    (do
      (println "Starting server...")
      (srv/run-server (app {:config {:base-url base-url}}) {:port 80})
      (println "Server started! API: " base-url))
    (do
      (println "Unable to start. Missing required environment variable: API_BASE_URL")
      (System/exit 1))))
