(ns ardoq.swagger.api
  (:require [ardoq.swagger.sync-swagger :as sync-swagger]
            [ardoq.client :as c]
            [ardoq.swagger.util :refer [parse-swagger]]
            [ardoq.swagger.validate :as validate]
            [ardoq.swagger.socket :refer [handler socket-send socket-close]]
            [org.httpkit.server :as srv]
            [clojure.data.json :as json]
            [compojure.core :refer [routes POST GET]]
            [superstring.core :as str]
            [clojure.java.io :as io]
            [cheshire.core :refer [generate-string parse-string]]
            [ring.util.response :refer [redirect-after-post response]]
            [clostache.parser :as tpl]
            [yaml.core :as yaml]
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
    (when-not (str/blank? headers)
      (json/read-str headers))
    (catch Exception e
      (throw (IllegalArgumentException. "Invalid headers (must be valid JSON)")))))

(defn get-resource-listing [url headers]
  (println "Importing specification from " url ". Custom headers" headers)
  (let [{:keys [status body] :as resp} (http/get (str (io/as-url url)) {:headers headers :insecure? true})]
    (println "Response" status "from" url)
    (if (= 200 status)
      (parse-swagger body)
      (throw (IllegalArgumentException. (str "Unexpected response " status " from " url))))))

(defn- resolve-spec [spec-text url headers]
  (if (not (str/blank? spec-text))
    (parse-swagger spec-text)
    (get-resource-listing url headers)))

(defn get-spec [client url wsname headers spec-text]
  ;;if spec is not null then use that as spec
  (let [spec (resolve-spec spec-text url headers)
        {:keys [swagger openapi]} spec
        spec-version (cond
                       openapi :openapi-3.x
                       (= swagger "2.0") :swagger-2.x
                       :else :swagger-1.x)
        wsname (cond (not (str/blank? wsname)) wsname
                     (not (str/blank? (:title spec))) (:title spec)
                     :default (str (name spec-version) " - import - " (.format (java.text.SimpleDateFormat. "yyyy.MM.dd HH:mm") (new java.util.Date))))]
    (sync-swagger/sync-swagger client spec wsname spec-version)))

(defn send-success-email! [wid org session client]
  (let [url (str (:url client) "/api/user/notify/email")]
    (try 
      (->> {:subject "Workspace is ready" :body (str "Your Open API (Swagger) workspace is fully imported.\nYou can visit it at " (:url client) "/app/view/workspace/" wid "?org=" org "\nRegards Ardoq")} 
           (generate-string)
           (assoc (:options client) :body)
           (http/post url))
      (catch Exception e
        (println "Failed to send e-mail")))))

(defn send-failure-email! [session client e]
  (let [url (str (:url client) "/api/user/notify/email")]
    (try
      (->> {:subject "Swagger import failed" :body (str "Your Open API (Swagger) workspace failed to import.\n" e)} 
           (generate-string)
           (assoc (:options client) :body)
           (http/post url))
      (catch Exception e
        (println "Failed to send e-mail")))))

(defn swagger-api [{:keys [config] :as system}]
  (routes
   (route/resources "/public")
   (GET "/socket" {}
        (partial handler system))
   (GET "/" {session :session
             headers :headers
             {:strs [org token]} :query-params :as request}
        {:status 200
         :body (tpl/render-resource "form.html" {:org-set (boolean org)
                                                 :org org
                                                 :token-set (boolean token)
                                                 :token token})
         :headers {"Content-Type" "text/html"}
         :session (-> session
                      (assoc :referer-host (some->> (get headers "referer") (re-find #"^https?://[^/]+")))
                      (assoc :x-forwarded-host (some->> (get headers "X-Forwarded-Host") (re-find #"^https?://[^/]+"))))})
   (POST "/import" {{:strs [url token org wsname headers swag notifier] :as params} :form-params session :session :as request}
         (let [client (c/client {:url (:base-url config)
                                 :org org
                                 :token token})]
           (prn "importing" url org wsname client)
           (try
             (let [wid (get-spec client url wsname (read-headers headers) swag)]
               (socket-close)
               (when notifier
                 (send-success-email! wid org session client))
               (str (:url client) "/app/view/workspace/" wid))
             (catch com.fasterxml.jackson.core.JsonParseException e
               (.printStackTrace e)
               (when notifier
                 (send-failure-email! session client "Failed to parse swagger endpoint"))
               {:status 400
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (str "Unable to parse swagger endpoint.")})})
             (catch IllegalArgumentException e
               (.printStackTrace e)
               (when notifier
                 (send-failure-email! session client (str "Failed reading request " (.getMessage e))))
               {:status 500
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (.getMessage e)})})
             (catch clojure.lang.ExceptionInfo e
               (.printStackTrace e)
               (when notifier
                 (send-failure-email! session client (str "An unexpected error occured!")))
               (cond
                 (-> e ex-data :mapping-key)
                   {:status 404
                    :headers {"Content-Type" "application/json"}
                    :body (json/write-str {:error (str "Failed parsing document under " (-> e ex-data :mapping-key))})}
                 (= 404 (-> e ex-data :status))
                   {:status 404
                    :headers {"Content-Type" "application/json"}
                    :body (json/write-str {:error (str (-> e ex-data :trace-redirects first) " returned 404")})}
                 :else
                   {:status 422
                    :headers {"Content-Type" "application/json"}
                    :body (json/write-str {:error (-> e ex-data :causes)})}))
             (catch Exception e
               (.printStackTrace e)
               (when notifier
                 (send-failure-email! session client "An unexpected error occured!"))
               {:status 500
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (str "An unexpected error occurred! ")})}))))))
