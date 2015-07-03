(ns ardoq.swagger.scrape
  (:require [ardoq.swagger.api :as api]
            [ardoq.swagger.client :as c]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [cheshire.core :refer [parse-string]]
            ))

(defn find-keys [spec keyword]
  ;;Finds all references in a given model
  (keep keyword (tree-seq #(or (map? %) (vector? %)) identity spec)))


(def client (c/client {:url "http://127.0.0.1:8080"
                       :token "2330f05eac3846f78a13b01930099b97"
                       :org "ardoq"}))

(defn get-company [{:keys [apis]}]
  (println apis)
  (doseq [k apis]
    (doseq [prop (get-in k [:properties])]
      (if (= (get-in prop [:type]) "Swagger")
        (try
          (api/get-spec client (get-in prop [:url]) nil nil nil)
          (catch Exception e (ex-data e)))))))

(def url-list ["3scale" "alchemyapi" "altos-data-geeks" "angellist" "bistri" "crowdin" "digitalgov-search" "dossia-" "dwolla" "easycron" "eventbrite" "expedia" "fueleconomygov" "gis-cloud" "google" "help-scout" "information-sharing-environment" "linkedin" "meetup-api" "nounproject-dev-team" "opencorporates" "paypal" "pinboard" "reddit" "runscope" "slack-api" "soundcloud-api" "stack-exchange" "twitter"])

(defn scrape-swaggers []
  (doseq [url url-list]
    (let [full-url (str "http://theapistack.com/data/" url "/apis.json")
          listing (into [] (api/get-resource-listing full-url nil))
          urls (find-keys listing (keyword "swagger-url"))]
      (doseq [url urls]
        (if (not (clojure.string/blank? url))
          (try
            (api/get-spec client url nil nil nil)
            (catch Exception e (println (str "Failed retrieving " url "Exception: " (.getMessage e)))))))
      (get-company (into {} listing)))))

(defn get-selected [selected]
  (api/get-spec client selected nil nil nil))
