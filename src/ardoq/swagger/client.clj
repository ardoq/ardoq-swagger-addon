(ns ardoq.swagger.client
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]))


(defprotocol ArdoqResource
  (resource-path [this]))

(defrecord Workspace [name description componentModel]
  ArdoqResource
  (resource-path [_] "workspace"))

(defrecord Component [name description rootWorkspace model typeId parent]
  ArdoqResource
  (resource-path [_] "component"))

(defrecord Model [name description]
  ArdoqResource
  (resource-path [_] "model"))

(defrecord Reference [rootWorkspace source target]
  ArdoqResource
  (resource-path [_] "reference"))

(defrecord Field [label name type model componentType]
  ArdoqResource
  (resource-path [_] "field"))

(defrecord Tag [name description rootWorkspace components references]
  ArdoqResource
  (resource-path [_] "tag"))


(defn client [{:keys [url token org]}]
  (let [default-options {:timeout 2000
                         :query-params {:org org}}
        client {:url url
                :options (merge-with merge default-options {:headers {"Authorization" (str "Token token=" token)
                                                                      "Content-Type" "application/json"
                                                                      "User-Agent" "ardoq-clojure-client"}})}]
    client))

(defn ok? [status]
  (and (< status 300)
       (> status 199)))

(defn record-ctor [rclass]
  (fn [map]
    (-> rclass
        (.getMethod "create" (into-array [clojure.lang.IPersistentMap]))
        (.invoke nil (object-array [map])))))

(defn- coerce-response [resource data]
  ((record-ctor (class resource)) data))

(defn find-by-id [resource client]
  (let [url (str (:url client) "/api/" (resource-path resource) "/" (:_id resource))
        {:keys [status body]} (http/get url (:options client))]
    (if (ok? status) 
      (coerce-response resource (json/read-json body true))
      (throw (ex-info "client-exception" {:status status :body body})))))

(defn find-aggregated [resource client]
  (let [url (str (:url client) "/api/" (resource-path resource) "/" (:_id resource) "/aggregated")
        {:keys [status body]} (http/get url (:options client))]
    (if (ok? status) 
      (coerce-response resource (json/read-json body true))
      (throw (ex-info "client-exception" {:status status :body body})))))

(defn find-in-workspace [resource client root-id]
  (let [url (str (:url client) "/api/" (resource-path resource) "/")
        {:keys [status body]} (http/get url (:options client))]
    (if (ok? status) 
      (filter #(= (:rootWorkspace %) root-id) 
              (map (partial coerce-response resource) (json/read-json body true)))
      (throw (ex-info "client-exception" {:status status :body body})))))

(defn find-all [resource client]
  (let [url (str (:url client) "/api/" (resource-path resource) "/")
        {:keys [status body]} (http/get url (:options client))]
    (if (ok? status) 
      (map (partial coerce-response resource) (json/read-json body true))
      (throw (ex-info "client-exception" {:status status :body body})))))

(defn create [resource client]
  (let [url (str (:url client) "/api/" (resource-path resource))
        {:keys [status body]} (http/post url (assoc (:options client) :body (json/write-str resource)))]
    (if (ok? status) 
      (coerce-response resource (json/read-json body true))
      (throw (ex-info "client-exception" {:status status :body body})))))

(defn update [resource client]
  (let [url (str (:url client) "/api/" (resource-path resource) "/" (:_id resource))
        {:keys [status body]} (http/put url (assoc (:options client) :body (json/write-str resource)))]
    (if (ok? status) 
      (coerce-response resource (json/read-json body true))
      (throw (ex-info "client-exception" {:status status :body body})))))

(defn delete [resource client]
  (let [url (str (:url client) "/api/" (resource-path resource) "/" (:_id resource))
        {:keys [status body]} (http/delete url (:options client))]
    (if-not (ok? status)
      (throw (ex-info "client-exception" {:status status :body body})))))
