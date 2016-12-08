(ns ardoq.swagger.swagger
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.socket :refer [socket-send]]
            [cheshire.core :refer [generate-string parse-string]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojurewerkz.urly.core :as urly]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]
            [clj-http.client :as http]))

(def ^:dynamic *custom-headers* {})

(defn html-to-markdown [string]
  (doto string
    (.replaceAll "<h\\d>(.*?)</h\\d>" "###$1\n")
    (.replaceAll "<b>(.*?)</b>" "**$1**")
    (.replaceAll "<i>(.*?)</i>" "*$1*")
    (.replaceAll "<pre>(.*?)</pre>" "```$1```")
    (.replaceAll "<code>(.*?)</code>" "```\n$1\n```")
    (.replaceAll "<p>(.*?)</p>" "\n$1\n\n")
    (.replaceAll "<br.*?>" "\n")
    (.replaceAll "\\{@code(.*?)\\}" "```$1```")
    (.replaceAll "</{0,1}\\w+?>" "")
    (.replaceAll "<\\w+/{0,1}?>" "")
    (.replaceAll "[<\u003c]" "&lt;")
    (.replaceAll "[>\u003e]" "&gt;")))


(defn model-template [m]
  (str "###JSON Schema\n```json\n"
       (generate-string m {:pretty true})
       "\n```"))

(defn create-workspace [client url base-url title model {:keys [info] :as data}]
  (let [name (if (s/blank? title)
               (or (:title info) base-url)
               title)]
    (-> (api/->Workspace name (tpl/render-resource "infoTemplate.tpl" (assoc info :workspaceName name :baseUrl base-url)) (str (:_id model)))
        (assoc :views ["swimlane" "sequence" "integrations" "componenttree" "relationships" "tableview" "tagscape" "reader" "processflow"])
        (api/create  client))))

(defn get-resource-listing [url]
  (let [{:keys [status body] :as resp} (http/get (str (io/as-url url)) {:headers *custom-headers* :insecure? true})]
    (println "\nResponse from " url "\n")
    (if (= 200 status)
      (parse-string body true)
      (throw (IllegalArgumentException. (str "Unexpected response " status " from " url))))))

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
              (assoc
                  (api/->Component type (model-template schema) (str wid) model-id (api/type-id-by-name model "Model")  nil)
               :schema schema)))
     {}
     (:models api-declaration))))

(defn create-operations [client {wid :_id model-id :componentModel :as w} parent model models {:keys [path operations]}]
  (map
   (fn [{:keys [method summary notes type items parameters] :as data}]
     (-> (api/map->Component {:name (str method " " path)
                              :description (common/generate-operation-description data models)
                              :rootWorkspace (str wid)
                              :model model-id
                              :parent (str (:_id parent))
                              :method method
                              :typeId (api/type-id-by-name model "Operation")})
         (api/create client)
         (assoc :return-model (keyword type)
                :input-models (set (map keyword (keep :type parameters))))))
   operations))

(defn create-api [client base-url workspace model models {:keys [resource component]}]
  (let [url (str base-url (:path resource))
        api-declaration (get-resource-listing url)]
    (common/update-comp client component api-declaration)
    (mapcat (partial create-operations client workspace component model models) (:apis api-declaration))))

(defn find-nested-model-deps [model]
  (letfn [(keys-in [m] (if (map? m)
                         (vec
                          (mapcat (fn [[k v]]
                                    (let [sub (keys-in v)
                                          nested (map #(into [k] %) (filter (comp not empty?) sub))]
                                      (if (seq nested)
                                        nested
                                        [[k]])))
                                  m))
                         []))]
    (keep #(let [r (last %)]
             (when (= :$ref r)
               (get-in model %)))
          (keys-in model))))

(defn interdependent-model-refs [client models]
  ;;Creates refs between models
  (doall (mapcat
          (fn [model]
            (let [rrr (find-nested-model-deps model)]
              (doall (keep
                      (fn [model-key]
                        (if-let [m ((keyword model-key) models)]
                          (-> (api/map->Reference {:rootWorkspace (:rootWorkspace model)
                                                   :source (str (:_id model))
                                                   :target (str (:_id m))
                                                   :type 3})
                              (api/create client))))
                      rrr))))
          (vals models))))

(defn create-refs [client operations models]
  (concat (mapcat
           (fn [{input-models :input-models return-model :return-model id :_id :as comp}]
             (let [input-refs
                   (keep (fn [k]
                           (if-let [m (k models)]                            
                             (-> (api/map->Reference {:rootWorkspace (:rootWorkspace comp)
                                                      :source (str id)
                                                      :target (str (:_id m))
                                                      :type 1})
                                 (api/create client))))
                         input-models)]
               (if (and return-model
                        (return-model models))
                 (conj input-refs
                       (-> (api/map->Reference {:rootWorkspace (:rootWorkspace comp)
                                                :source (str id)
                                                :target (str (:_id (return-model models)))
                                                :type 0})
                           (api/create client)))
                 input-refs)))
           operations)
          (interdependent-model-refs client models)))

(defn resolve-url [{:keys [basePath] :as all} url]
  (if (some? basePath)
    (let [u (urly/url-like basePath)]
      (cond
        (urly/absolute? u) basePath
        (urly/relative? u) (str (.mutatePath (urly/url-like url) (urly/path-of u)))))
    url))

(defn update-resource [client workspace url model resource component]
  {:resource resource
   :component (-> (assoc component :description (:description resource))
                  (api/map->Component)
                  (api/update client))})

(defn update-resources [client workspace url model resource]
  (or (some->> (first (filter #(and (= (:path resource) (:name %)) (= (:type %) "Resource")) (:components workspace)))
               (update-resource client workspace url model resource))
      (create-resource client workspace url model resource)))

(defn get-operations-and-models [acc resource]
  (assoc acc :models (into (:models acc) (:models resource)) :operations (into (:operations acc) (:apis resource))))

(defn delete-operations-and-models [client {components :components :as workspace} base-url resources]
  (let [resource-listing (doall (reduce (fn [acc resource]
                                          (->> (:path resource)
                                               (str base-url)
                                               (get-resource-listing)
                                               (get-operations-and-models acc)))
                                        {}
                                        resources))]
    (doseq [{def-name :name :as component} components]
      (if (= (:type component) "Model") 
        (when-not (first (filter #(= (name %) def-name) (keys (:models resource-listing)))) 
          (api/delete (api/map->Component component) client)))
      
      (if (= (:type component) "Operation")        
        (let [op (flatten (doall (map (fn [{path :path operations :operations}]
                                        (doall (map (fn [operation]
                                                      (str (:method operation) " " path)) 
                                                    operations)))
                                      (:operations resource-listing))))]
          (when-not (first (filter #(= % def-name) op))
            (api/delete (api/map->Component component) client)))))))

(defn delete-resources [client {components :components :as workspace} resources]
  (doseq [{def-name :name :as component} components]
    (if (= (:type component) "Resource") 
      (when-not (first (filter #(= (:path %) def-name) resources))
        (api/delete (api/map->Component component) client)))))

(defn update-model [client base-url {wid :_id model-id :componentModel :as workspace} model {:keys [resource component]}]
  (let [url (str base-url (:path resource))
        api-declaration (get-resource-listing url)]
    (reduce 
     (fn [acc [type schema]]
       (assoc acc (keyword type) 
              (or (some-> (first (filter #(and (= (:name %) (name type)) (= (:type %) "Model")) (:components workspace)))
                          (assoc :description (model-template schema))
                          (api/map->Component)
                          (assoc :schema schema))
                  (assoc
                      (api/->Component type (model-template schema) (str wid) model-id (api/type-id-by-name model "Model")  nil)
                    :schema schema))))
     {}
     (:models api-declaration))))

(defn create-or-update-operations [client {wid :_id model-id :componentModel :as workspace} parent model models {:keys [path operations]}]
  (map
   (fn [{:keys [method summary notes type items parameters] :as data}]
     (or (some-> (first (filter #(and (= (:name %) (str method " " path)) 
                                      (= (:type %) "Operation")
                                      (= (:parent %) (:_id parent))) 
                                (:components workspace)))
                 (api/map->Component)
                 (assoc :description (common/generate-operation-description data models))
                 (assoc :return-model (keyword type)
                        :input-models (set (map keyword (keep :type parameters)))))
         (-> (api/map->Component {:name (str method " " path)
                                  :description (common/generate-operation-description data models)
                                  :rootWorkspace (str wid)
                                  :model model-id
                                  :parent (str (:_id parent))
                                  :method method
                                  :typeId (api/type-id-by-name model "Operation")})
             (api/create client)
             (assoc :return-model (keyword type)
                    :input-models (set (map keyword (keep :type parameters)))))))
   operations))

(defn update-operations [client base-url workspace model models {:keys [resource component]}]
  (let [url (str base-url (:path resource))
        api-declaration (get-resource-listing url)]
    (common/update-comp client component api-declaration)
    (mapcat (partial create-or-update-operations client workspace component model models) (:apis api-declaration))))

(defn delete-and-create-refs [client {references :references :as workspace} operations models]
  (doseq [ref references]
    (when (= (:rootWorkspace ref) (:targetWorkspace ref) (:_id workspace))
      (api/delete (api/map->Reference ref) client)))
  (create-refs client operations models))

(defn update-swagger [workspace client resource-listing url model]
  (socket-send (str "Found workspace " (:name workspace) "\nUpdating " (count (:apis resource-listing)) " resources"))
  (let [resources (doall (map (partial update-resources client workspace url model) (:apis resource-listing)))
        _ (socket-send (str "Updated " (count resources) " resources\nUpdating models"))
        models (doall (-> (apply merge (map (partial update-model client url workspace model) resources)) 
                          (common/save-models client workspace)))
        _ (socket-send (str "Updated " (count models) " models\nUpdating operations"))
        operations (doall (mapcat (partial update-operations client url workspace model models) resources))
        _ (socket-send (str "Updated " (count operations) " operations\nUpdating references"))
        refs (delete-and-create-refs client workspace operations models)
        _ (socket-send (str "Updated " (count refs) " refs\n Starting pruning of resources"))
        workspace (api/find-aggregated workspace client) ;Getting an updated version of the workspace
        all {:workspace workspace
             :resources resources
             :models models
             :operations operations
             :refs refs}]
    (delete-resources client workspace (:apis resource-listing))
    (socket-send "Done pruning resources\nStarting pruning of operations")
    (delete-operations-and-models client workspace url (:apis resource-listing))
    (socket-send "Done purning operationss")
    (common/find-or-create-fields client model)
    (str (:_id workspace))))

(defn import-swagger [client resource-listing base-url name headers]
  (binding [*custom-headers* headers]
    (let [url (resolve-url resource-listing base-url)
          model (common/create-model client :swagger-1.x)]
      (socket-send "Got Swagger 1 model")
      (when-not (some-> (common/find-existing-resource client (if (s/blank? name) (or (:title resource-listing) base-url) name) #(api/map->Workspace {}))
                        (api/find-aggregated client)
                        (update-swagger client resource-listing url model))
        (let [workspace (create-workspace client url base-url name model resource-listing)
              _ (socket-send (str "Created workspace " (or name (:title (:info resource-listing)) base-url) "\n Creating " (count (:apis resource-listing)) " resources"))
              resources (doall (map (partial create-resource client workspace url model) (:apis resource-listing)))
              _ (socket-send (str "Created " (count resources) " resources\nCreating models"))
              models (doall (-> (apply merge (map (partial create-models client url workspace model) resources))
                                (common/save-models client nil)))
              _ (socket-send (str "Created " (count models) " models\nCreating operations"))
              operations (doall (mapcat (partial create-api client url workspace model models) resources))
              _ (socket-send (str "Created " (count operations) " operations\nCreating references"))
              refs (doall (create-refs client operations models))
              _ (socket-send (str "Created " (count refs) " refs"))
              all {:workspace workspace
                   :resources resources
                   :models models
                   :oerations operations
                   :refs refs}]
          (common/find-or-create-fields client model)
          (socket-send (str "Imported " (count resources) " resources, " (count models) " json schemas," (count operations) " operations and " (count refs) " refs."))
          (str (:_id workspace)))))))
