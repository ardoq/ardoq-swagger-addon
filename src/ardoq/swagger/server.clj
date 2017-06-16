(ns ardoq.swagger.server
  (:require [ardoq.swagger.api :refer [swagger-api]]
            [org.httpkit.server :as srv]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.session :refer [wrap-session]])
  (:gen-class :main true))

(defn app [system]
  (-> system
      swagger-api
      (wrap-session {:cookie-name "reffie"})
      wrap-params))


(defn start-server [system]
  (srv/run-server (app system) system))

(defn -main []
  (if-let [base-url (or (.. System (getProperties) (get "API_BASE_URL"))
                        (System/getenv "API_BASE_URL"))]
    (do
      (println "Starting server...")
      (srv/run-server (app {:port 80
                            :config {:base-url base-url}}) {})
      (println "Server started! API: " base-url))
    (do
      (println "Unable to start. Missing required environment variable: API_BASE_URL")
      (System/exit 1))))
