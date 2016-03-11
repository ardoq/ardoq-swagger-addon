(ns ardoq.swagger.api
  (:require [ardoq.swagger.swagger :as swagger]
            [ardoq.swagger.swagger-v2 :as swaggerv2]
            [ardoq.swagger.client :as c]
            [ardoq.swagger.validate :as validate]
            [ardoq.swagger.socket :refer [handler socket-send socket-close]]
            [org.httpkit.server :as srv]
            [clojure.data.json :as json]
            [compojure.core :refer [routes POST GET]]
            [clojure.string :refer [blank?]]
            [clojure.java.io :as io]
            [cheshire.core :refer [generate-string parse-string]]
            [ring.util.response :refer [redirect-after-post response]]
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

(defn version1 [client spec url name headers ignore-validate]
  (if ignore-validate
    (do (socket-send "Ignoring validation - Importing Swagger 1" false)
        (swagger/import-swagger client spec url name headers))
    (let [{:keys [success message]} (validate/validate-swagger "schemav1.json" (generate-string spec))]
      (if success
        (do (socket-send "Valid Swagger - Importing Swagger 1" false)
            (swagger/import-swagger client spec url name headers)
            (println "Done importing swagger doc from " url "."))
        (do (println "Not a valid Swagger file\nError: ") 
            (socket-close)
            (throw (ex-info  "InvalidSwagger" {:causes message})))))))

(defn version2 [client spec wsname ignore-validate]
  (if ignore-validate
    (do (socket-send "Ignoring validation - Importing Swagger 2")
        (swaggerv2/import-swagger2 client spec wsname))
    (let [{:keys [success message]} (validate/validate-swagger "schemav2.json" (generate-string spec))]
      (if success 
        (do (socket-send "Valid Swagger - Importing Swagger 2")
            (swaggerv2/import-swagger2 client spec wsname))
        (do (println "Not a valid Swagger file\nError: ") 
            (socket-close)
            (throw (ex-info "InvalidSwagger" {:causes message})))))))

(defn- resolve-spec [spec url headers]
  (if (not (blank? spec))
    (parse-string spec true)
    (get-resource-listing url headers)))

(defn get-spec [client url wsname headers spec ignore-validate]
  ;;if swag is not null then use that as spec
  (let [{:keys [swagger] :as spec} (resolve-spec spec url headers)]
    (if (= swagger "2.0")
      (version2 client spec wsname ignore-validate)
      (version1 client spec url wsname headers ignore-validate))))

(defn swagger-api [{:keys [config] :as system}]
  (routes
   (route/resources "/public")
   (GET "/socket" {} 
        (partial handler system))
   (GET "/" {session :session
             headers :headers
             {:strs [org token]} :query-params :as request}         
        {:status 200
         :body (tpl/render-resource "form.html" {:org-set (boolean org) :org org 
                                          :token-set (boolean token)
                                          :token token})
         :headers {"Content-Type" "text/html"}
         :session (assoc session :referer (if (get headers "referer") (str "http://" (first (rest (rest (.split (get headers "referer") "/"))))) ""))})
   (POST "/import" {{:strs [url token org wsname headers swag ignorer] :as params} :form-params session :session :as request}
         (try
           (let [client (c/client {:url (:base-url config)
                                   :org org
                                   :token token})
                 wid (get-spec client url wsname (read-headers headers) swag ignorer)]
             (socket-close)
             (str (:referer session) "/app/view/workspace/" wid "?org=" org))
           (catch com.fasterxml.jackson.core.JsonParseException e
             (.printStackTrace e)
             {:status 406
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (str "Unable to parse swagger endpoint.")})})
           (catch IllegalArgumentException e
             (.printStackTrace e)
             {:status 406
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (.getMessage e)})})
           (catch clojure.lang.ExceptionInfo e
             (.printStackTrace e)
             (if (= 404 (-> e ex-data :status))
               {:status 404
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (str (-> e ex-data :trace-redirects first) " returned 404")})}
               {:status 406
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (-> e ex-data :causes)})}))
           (catch Exception e
             (.printStackTrace e)
             {:status 500
              :headers {"Content-Type" "application/json"}
              :body (json/write-str {:error (str "An unexpected error occurred! ")})})))))
