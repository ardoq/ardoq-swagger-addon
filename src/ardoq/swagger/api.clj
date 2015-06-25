(ns ardoq.swagger.api
  (:require [ardoq.swagger.swagger :as swagger]
            [ardoq.swagger.swagger-v2 :as swaggerv2]
            [ardoq.swagger.client :as c]
            [clojure.data.json :as json]
            [compojure.core :refer [routes POST GET]]
            [clojure.string :refer [blank?]]
            [ring.util.response :refer [redirect-after-post]]
            [clostache.parser :as tpl]
            [hiccup.core :refer [html]]
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

(defn swagger-api [{:keys [config]}]
  (routes
   (route/resources "/public")
   (GET "/" {{:strs [org token]} :query-params} 
        (tpl/render-resource "form.html" {:org-set (boolean org) :org org 
                                          :token-set (boolean token)
                                          :token token}))
   (GET "/v2.html" {{:strs [org token]} :query-params} 
        (tpl/render-resource "v2.html" {:org-set (boolean org) :org org 
                                          :token-set (boolean token)
                                          :token token}))
   ;(POST "/import/v1")
   (POST "/import/v1" {{:strs [url token org wsname headers] :as params} :form-params}
         (try
           (let [wid (swagger/import-swagger (c/client {:url (:base-url config)
                                                        :org org
                                                        :token token})
                                             url
                                             wsname
                                             (read-headers headers))]
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
              :body (json/write-str {:error (str "An unexpected error occurred! ")})})))

   ;;Do post to v2 here
   (POST "/import/v2" {{:strs [url token org wsname headers swag] :as params} :form-params}
         (try
           (let [wid (swaggerv2/import-swagger2 (c/client {:url (:base-url config)
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
