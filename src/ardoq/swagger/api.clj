(ns ardoq.swagger.api
  (:require [ardoq.swagger.swagger :as swagger]
            [ardoq.swagger.swagger-v2 :as swaggerv2]
            [ardoq.swagger.client :as c]
            [clojure.data.json :as json]
            [compojure.core :refer [routes POST GET]]
            [clojure.string :refer [blank?]]
            [clojure.java.io :as io]
            [ring.util.response :refer [redirect-after-post]]
            [cheshire.core :refer [generate-string parse-string]]
            [clostache.parser :as tpl]
            [hiccup.core :refer [html]]
            [org.httpkit.client :as http]
            [hiccup.form :refer [form-to submit-button text-field label hidden-field]]
            [compojure.route :as route]))

(defn- content-type
  "Return the content-type of the request, or nil if no content-type is set."
  [request]
  (if-let [type (get-in request [:headers "content-type"])]
    (second (re-find #"^(.*?)(?:;|$)" type))))

(defn read-headers [headers]
  (try
    (when-not (blank? headers)
      (json/read-str headers))
    (catch Exception e
      (throw (IllegalArgumentException. "Invalid headers (must be valid JSON)")))))

(defn get-resource-listing [url headers]
  (println "Importing swagger doc from " url ". Custom headers" headers)
  (let [{:keys [status body] :as resp} @(http/get (str (io/as-url url)) {:headers headers :insecure? true})]
    (println "\nResponse from " url "\n")
    (if (= 200 status)
      (parse-string body true)
      (throw (IllegalArgumentException. (str "Unexpected response " status " from " url))))))

(defn version2? [{:keys [swagger]}]
  (if (and swagger (= swagger "2.0")) 
      true
      false))

(def client (c/client {:url "http://127.0.0.1:8080"
                       :token "2330f05eac3846f78a13b01930099b97"
                       :org "ardoq"}))

(defn get-spec [client url name headers swag]
  ;if swag is not null then use that as spec
  (let [spec (if (not (blank? swag)) (parse-string swag true) (get-resource-listing url headers))]
    (if (version2? spec)
      (swaggerv2/import-swagger2 client spec name)
      (swagger/import-swagger client spec url name headers)))
  (println "Done importing swagger doc from " url "."))

(defn swagger-api [{:keys [config]}]
  (routes
   (route/resources "/public")
   (GET "/" {{:strs [org token]} :query-params} 
        (tpl/render-resource "form.html" {:org-set (boolean org) :org org 
                                          :token-set (boolean token)
                                          :token token}))

   ;(POST "/import")
   (POST "/import" {{:strs [url token org wsname headers swag] :as params} :form-params}
         (try
           (let [wid (get-spec (c/client {:url (:base-url config)
                                                        :org org
                                                        :token token})
                                             url
                                             wsname
                                             (read-headers headers)
                                             swag)]
             (str (:base-url config) "/app/view/workspace/" wid "?org=" org))
           (catch com.fasterxml.jackson.core.JsonParseException e
             (.printStackTrace e)
             {:status 406
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (str "Unable to parse swagger endpoint.")})})
           (catch IllegalArgumentException e
             {:status 406
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (.getMessage e)})})
           (catch Exception e
             (.printStackTrace e)
             {:status 500
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (str "An unexpected error occurred! ")})})))))
