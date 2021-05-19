(ns ardoq.swagger.api
  (:require [ardoq.client :as c]
            [ardoq.swagger.socket :refer [handler socket-close]]
            [ardoq.swagger.sync-swagger :as sync-swagger]
            [ardoq.swagger.util :refer [parse-swagger]]
            [ardoq.version :as version]
            [cheshire.core :refer [generate-string]]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clostache.parser :as tpl]
            [compojure.core :refer [routes POST GET]]
            [compojure.route :as route]
            [superstring.core :as str]))

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

(defn synchronize-specification [client url wsname headers spec-text overview]
  ;;if spec is not null then use that as spec
  (let [spec (resolve-spec spec-text url headers)
        spec-title (get-in spec [:info :title])
        spec-info-version (get-in spec [:info :version])
        spec-version (cond
                       (:openapi spec) :openapi-3.x
                       (= (:swagger spec) "2.0") :swagger-2.x
                       :else :swagger-1.x)
        wsname (cond (not (str/blank? wsname))
                        wsname
                     (and (not (str/blank? spec-title)) (not (str/blank? spec-info-version)))
                        (str spec-title " - " spec-info-version)
                     (not (str/blank? spec-title))
                        spec-title
                     :default
                        (str (name spec-version) " - import - " (.format (java.text.SimpleDateFormat. "yyyy.MM.dd HH:mm") (new java.util.Date))))]
    (sync-swagger/sync-swagger client spec wsname spec-version overview)))

(defn send-success-email! [wid org client]
  (let [url (str (:url client) "/api/user/notify/email")]
    (try 
      (->> {:subject "Workspace is ready" :body (str "Your Open API (Swagger) workspace is fully imported.\nYou can visit it at " (:url client) "/app/view/workspace/" wid "?org=" org "\nRegards Ardoq")} 
           (generate-string)
           (assoc (:options client) :body)
           (http/post url))
      (catch Exception e
        (println "Failed to send e-mail")))))

(defn send-failure-email! [client e]
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
   (GET "/status" {} {:status 200
                      :body version/string
                      })
   (GET "/" {{:strs [org]} :query-params :as request}
        {:status 200
         :body (tpl/render-resource "form.html" {:org-set (boolean org)
                                                 :org org})
         :headers {"Content-Type" "text/html"}})
   (POST "/import" {params :form-params
                    multipart-params :multipart-params
                    cookies :cookies
                    :as request}
     (let [merged-params (merge params multipart-params)
           ring-session-value (get-in cookies ["ring-session" :value])
           _ (println "Swagger debug: Cookies" cookies)
           _ (println "Swagger debug: Parsed ring session value:" ring-session-value)
           {:strs [url org swag wsname headers notifier overview-ws overview-comp-type overview-ref-type]} merged-params
           client (c/client {:url (:base-url config)
                             :ring-session ring-session-value
                             :org org})]
           (prn "importing" url org wsname client overview-ws overview-comp-type overview-ref-type)
           (try
             (let [sync-status (synchronize-specification client url wsname (read-headers headers) swag {:overview-workspace overview-ws
                                                                                                         :overview-component-type overview-comp-type
                                                                                                         :overview-reference-type overview-ref-type})
                   wid (:workspace-id sync-status)]
               (socket-close)
               (when notifier
                 (send-success-email! wid org client))
               (str (:url client) "/app/view/workspace/" wid))
             (catch com.fasterxml.jackson.core.JsonParseException e
               (.printStackTrace e)
               (when notifier
                 (send-failure-email! client "Failed to parse swagger endpoint"))
               {:status 400
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (str "Unable to parse swagger endpoint.")})})
             (catch IllegalArgumentException e
               (.printStackTrace e)
               (when notifier
                 (send-failure-email! client (str "Failed reading request " (.getMessage e))))
               {:status 500
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (.getMessage e)})})
             (catch clojure.lang.ExceptionInfo e
               (.printStackTrace e)
               (when notifier
                 (send-failure-email! client (str "An unexpected error occured!")))
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
                 (send-failure-email! client "An unexpected error occured!"))
               {:status 500
                :headers {"Content-Type" "application/json"}
                :body (json/write-str {:error (str "An unexpected error occurred! ")})}))))))
