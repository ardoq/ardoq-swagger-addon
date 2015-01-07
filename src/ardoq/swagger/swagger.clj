(ns ardoq.swagger.swagger
  (:import [java.net URI URL])
  (:require
   [ardoq.swagger.client :as api]
   [clojurewerkz.urly.core :as urly]
   [org.httpkit.client :as http]
   [clostache.parser :as tpl]
   [clojure.java.io :as io]
   [cheshire.core :refer [generate-string parse-string]]))

(defn find-or-create-model [client]
  (if-let [model (first (filter #(= "Swagger" (:name %)) (api/find-all (api/map->Model {}) client)))]
    model
    (-> (api/map->Model (parse-string (slurp (io/resource "model.json")) true))
        (api/create client))))

(defn model-template [m]
  (str "###JSON Schema\n```\n"
       (generate-string m {:pretty true})
       "\n```"))

(defn create-workspace [client url base-url title model {:keys [info] :as data}]
  (let [name (or title (:title info))]
    (-> (api/->Workspace name (tpl/render-resource "infoTemplate.tpl" (assoc info :workspaceName name :baseUrl base-url)) (str (:_id model)))
        (api/create  client))))

(defn get-resource-listing [url]
  (let [{:keys [status body]} @(http/get (str (io/as-url url)))]
    (if (= 200 status)
      (parse-string body true)
      (throw IllegalArgumentException "Bad swagger url!"))))

(defn create-resource [client {wid :_id model-id :componentModel :as w} base-url model {:keys [path description] :as r}]
  {:resource r
   :component (-> (api/->Component path description (str wid) model-id (api/type-id-by-name model "Resource") nil)
                  (api/create client))})

(defn create-models [client base-url {wid :_id model-id :componentModel :as w} model {:keys [resource component]}]
  (let [url (str base-url (:path resource))
        api-declaration (get-resource-listing url)]
    (reduce
     (fn [acc [type schema]]
       (assoc acc (keyword type)
              (-> (api/->Component type (model-template schema) (str wid) model-id (api/type-id-by-name model "Model")  nil)
                  (api/create client))))
     {}
     (:models api-declaration))))

(defn update-comp [client component {:keys [produces]}]
  (-> (api/map->Component component)
      (assoc :produces produces)
      (api/update client)))

(defn create-operations [client {wid :_id model-id :componentModel :as w} parent model {:keys [path operations]}]
  (map
   (fn [{:keys [method summary notes type items parameters] :as data}]
     (-> (api/map->Component {:name (str method " " path)
                              :description (tpl/render-resource "operationTemplate.tpl"  data)
                              :rootWorkspace (str wid)
                              :model model-id
                              :parent (str (:_id parent))
                              :method method
                              :typeId (api/type-id-by-name model "Operation")
                              :swaggerModels (set (filter identity (concat [type (:$ref items)] (map :type parameters))))})
         (api/create client)))
   operations))

(defn create-api [client base-url workspace model {:keys [resource component]}]
  (let [url (str base-url (:path resource))
        api-declaration (get-resource-listing url)]
    (update-comp client component api-declaration)
    (mapcat (partial create-operations client workspace component model) (:apis api-declaration))))

(defn create-refs [client operations models]
  (mapcat
   (fn [{swagger-models :swaggerModels id :_id :as comp}]
     (keep (fn [k]
             (if-let [m (k models)]
               (-> (api/map->Reference {:rootWorkspace (:rootWorkspace comp)
                                        :source (str id)
                                        :target (str (:_id m))
                                        :type 0})
                   (api/create client))))
           (map keyword swagger-models)))
   operations))

(defn resolve-url [{:keys [basePath] :as all} url]
  (if (some? basePath)
    (let [u (urly/url-like basePath)]
      (cond
        (urly/absolute? u) basePath
        (urly/relative? u) (str (.mutatePath (urly/url-like url) (urly/path-of u)))))
    url))

(defn import-swagger [client base-url name]
  (println "Importing swagger doc from " base-url ".")
  (let [resource-listing (get-resource-listing base-url)
        url (resolve-url resource-listing base-url)
        model (find-or-create-model client)
        workspace (create-workspace client url base-url name model resource-listing)
        resources (doall (map (partial create-resource client workspace url model) (:apis resource-listing)))
        models (doall (apply merge (map (partial create-models client url workspace model) resources)))
        operations (doall (mapcat (partial create-api client url workspace model) resources))
        refs (doall (create-refs client operations models))
        all {:workspace workspace
             :resources resources
             :models models
             :operations operations
             :refs refs}]
    (println "Done importing swagger doc from " base-url ".")
    (println "Imported " (count resources) " resources, " (count models) " json schemas," (count operations) " operations and " (count refs) " refs.")
    (str (:_id workspace))))
