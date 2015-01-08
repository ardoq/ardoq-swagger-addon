(ns ardoq.swagger.api
  (:require [ardoq.swagger.swagger :as swagger]
            [ardoq.swagger.client :as c]
            [clojure.data.json :as json]
            [compojure.core :refer [routes POST GET]]
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

(defn swagger-api [{:keys [config]}]
  (routes
   (route/resources "/public")
   (GET "/" {{:strs [org token]} :query-params} 
        (tpl/render-resource "form.html" {:org-set (boolean org) :org org 
                                          :token-set (boolean token)
                                          :token token}))
   (POST "/import" {{:strs [url token org wsname] :as params} :form-params}
         (let [wid (swagger/import-swagger (c/client {:url (:base-url config)
                                                      :org org
                                                      :token token})
                                           url
                                           wsname)]
           (redirect-after-post (str (:base-url config) "/app/view/workspace/" wid "?org=" org))))))
