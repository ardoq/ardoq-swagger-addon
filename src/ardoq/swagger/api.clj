(ns ardoq.swagger.api
  (:require [ardoq.swagger.swagger :as swagger]
            [ardoq.swagger.client :as c]
            [clojure.data.json :as json]
            [compojure.core :refer [routes POST GET]]
            [ring.util.response :refer [redirect-after-post]]
            [hiccup.core :refer [html]]
            [hiccup.form :refer [form-to submit-button text-field label hidden-field]]))

(defn- content-type
  "Return the content-type of the request, or nil if no content-type is set."
  [request]
  (if-let [type (get-in request [:headers "content-type"])]
    (second (re-find #"^(.*?)(?:;|$)" type))))

(defn swagger-api [{:keys [config]}]
  (routes
   (GET "/" {{:strs [org token]} :query-params} (html (cond-> (form-to [:post "/import"]
                                                                       (label "url" "Url: ")
                                                                       (text-field "url")
                                                                       (label "wsname" "Workspace name: ")
                                                                       (text-field "wsname"))
                                                        token (conj (hidden-field "token" token))
                                                        (not token) (-> (conj (label "token" "API token: "))
                                                                        (conj (text-field "token")))
                                                        org (conj (hidden-field "org" org))
                                                        (not org) (-> (conj (label "org" "Organization label:"))
                                                                      (conj (text-field "org")))
                                                        true (conj (submit-button "Import")))))
   (POST "/import" {{:strs [url token org wsname] :as params} :form-params}
         (let [wid (swagger/import-swagger (c/client {:url (:base-url config)
                                                      :org org
                                                      :token token})
                                           url
                                           wsname)]
           (redirect-after-post (str (:base-url config) "/app/view/workspace/" wid "?org=" org))))))
