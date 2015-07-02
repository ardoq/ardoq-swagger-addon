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

(defn google-urls []
  (let [{:keys [apis]} (api/get-resource-listing "http://theapistack.com/data/google/apis.json" nil)]
    (doseq [k apis]
      (doseq [prop (get-in k [:properties])]
        (if (= (get-in prop [:type]) "Swagger")
          (api/get-spec client (get-in prop [:url]) nil nil nil))))))

(defn scrape-swaggers []
  (let [listing (into [] (api/get-resource-listing "http://stack.apievangelist.com/data/companies.json" nil))
        urls (find-keys listing (keyword "swagger-url"))]
    (doseq [url urls]
      (if (not (clojure.string/blank? url))
        (try
          (api/get-spec client url nil nil nil)
          (catch Exception e (println (str "Failed retrieving " url "Exception: " (.getMessage e))))))))
  (google-urls))

(defn get-selected [selected]
  (api/get-spec client selected nil nil nil))
