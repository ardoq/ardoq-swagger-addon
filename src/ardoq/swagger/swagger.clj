(ns ardoq.swagger.swagger
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]
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
  (str "###JSON Schema\n```\n"
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

(defn delete-and-create-refs [client workspace operations models]
  (doseq [ref (:references workspace)]
    (api/delete (api/map->Reference ref) client))
  (doall (create-refs client operations models)))

(defn update-resources [client workspace url model resource]
  "All of these should check if a resource is in workspace. If not then create otherwise update. Need to delete some as well."
  (or (some-> (filter #(and (= (:path resource) (:name %)) (= (:type %) "Resource")) (:components workspace))
              (str "Do and update here"))
      (create-resource client workspace url model resource)))

(defn update-model [client url workspace model resource]
  "This gets in a full resource from the previous method. Need to complete that before this one"
  ;; (or (some-> (filter #(and (= (:path resource) (:name %)) (= (:type %) "Model")) (:components workspace))
  ;;             (str "Do and update here"))
  ;;     (create-resource client workspace url model resource))
  "random return")

(defn update-operations [client url workspace model models]
  "This gets in a full resource from the previous method. Need to complete that before this one"
  ;; (or (some-> (filter #(and (= (:path resource) (:name %)) (= (:type %) "Model")) (:components workspace))
  ;;             (str "Do and update here"))
  ;;     (create-resource client workspace url model resource))
  "random return")

(defn update-swagger [workspace client resource-listing url model]
  (let [resources (doall (map (partial update-resources client workspace url model) (:apis resource-listing)))
        models (doall (-> (apply merge (map (partial update-model client url workspace model) resources))))
        ;operations (doall (mapcat (partial update-operations client url workspace model models) resources))
        ;refs (delete-and-create-refs client workspace operations models)
        ;; all
        ;; {:workspace workspace
        ;;  :resources resources
        ;;  :models models
        ;;  :operations operations
        ;;  :refs refs}
        ]
    ;(common/find-or-create-fields client model)
    (println "Done updating Swagger")
    (str (:_id workspace))))

(defn import-swagger [client resource-listing base-url name headers]
  (binding [*custom-headers* headers]
    (let [url (resolve-url resource-listing base-url)
          model (common/find-or-create-model client "Swagger")]
      (when-not (some-> (common/find-existing-resource client (if (s/blank? name) (or (:title resource-listing) base-url) name) #(api/map->Workspace {}))
                        (api/find-aggregated client)
                        (update-swagger client resource-listing url model))
        (let [workspace (create-workspace client url base-url name model resource-listing)
              resources (doall (map (partial create-resource client workspace url model) (:apis resource-listing)))
              models (doall (-> (apply merge (map (partial create-models client url workspace model) resources))
                                (common/save-models client)))
              operations (doall (mapcat (partial create-api client url workspace model models) resources))
              refs (doall (create-refs client operations models))
              all {:workspace workspace
                   :resources resources
                   :models models
                   :operations operations
                   :refs refs}]
          (common/find-or-create-fields client model)
          (println "Imported " (count resources) " resources, " (count models) " json schemas," (count operations) " operations and " (count refs) " refs.")
          (str (:_id workspace)))))))
