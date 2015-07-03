(ns ardoq.swagger.api
  (:require [ardoq.swagger.swagger :as swagger]
            [ardoq.swagger.swagger-v2 :as swaggerv2]
            [ardoq.swagger.client :as c]
            [ardoq.swagger.validate :as validate]
            [clojure.data.json :as json]
            [compojure.core :refer [routes POST GET]]
            [clojure.string :refer [blank?]]
            [clojure.java.io :as io]
            [ring.util.response :refer [redirect-after-post]]
            [cheshire.core :refer [generate-string parse-string]]
            [clostache.parser :as tpl]
            [hiccup.core :refer [html]]
            [clj-http.client :as http]
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
  (let [{:keys [status body] :as resp} (http/get (str (io/as-url url)) {:headers headers :insecure? true})]
    (println "\nResponse from " url "\n")
    (if (= 200 status)
      (parse-string body true)
      (throw (IllegalArgumentException. (str "Unexpected response " status " from " url))))))

(defn version1 [client spec url name headers]
  (let [{:keys [success message]} (validate/validate-swagger "schemav1.json" (generate-string spec))]
    (if (= success true)
      (do (println "Valid Swagger1")
          (swagger/import-swagger client spec url name headers)
          (println "Done importing swagger doc from " url "."))
      (do (println "Not a valid Swagger file\nError: ") 
          ;(clojure.pprint/pprint message)
          (throw (ex-info  "InvalidSwagger" {:causes message}))))))

(defn version2 [client spec name]
  (let [{:keys [success message]} (validate/validate-swagger "schemav2.json" (generate-string spec))]
    (if (= success true) 
      (do (swaggerv2/import-swagger2 client spec name)
          (println "Done importing swagger doc."))
      (do (println "Not a valid Swagger file\nError: ") 
          ;(clojure.pprint/pprint message)
          (throw (ex-info "InvalidSwagger" {:causes message}))))))

(defn get-spec [client url name headers swag]
  ;if swag is not null then use that as spec
  (let [{:keys [swagger] :as spec} (if (not (blank? swag)) (parse-string swag true) (get-resource-listing url headers))]
    (if (= swagger "2.0")
      (version2 client spec name)
      (version1 client spec url name headers))))

(defn swagger-api [{:keys [config]}]
  (routes
   (route/resources "/public")
   (GET "/" {{:strs [org token]} :query-params} 
        (tpl/render-resource "form.html" {:org-set (boolean org) :org org 
                                          :token-set (boolean token)
                                          :token token}))
   (POST "/import" {{:strs [url token org wsname headers swag] :as params} :form-params}
         (try
           (let [client (c/client {:url (:base-url config)
                                   :org org
                                   :token token})
                 wid (get-spec client url wsname (read-headers headers) swag)]
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
           (catch clojure.lang.ExceptionInfo e
             {:status 406
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (str (-> e ex-data :causes))})})
           (catch Exception e
             (.printStackTrace e)
             {:status 500
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (str "An unexpected error occurred! ")})})))))
