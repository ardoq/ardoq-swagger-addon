(ns ardoq.swagger.swagger
  (:import [java.net URI URL])
  (:require
   [ardoq.swagger.client :as api]
   [clojurewerkz.urly.core :as urly]
   [org.httpkit.client :as http]
   [clostache.parser :as tpl]
   [clojure.java.io :as io]
   [cheshire.core :refer [generate-string parse-string]]
   [clojure.string :as s]
   [medley.core :refer [map-vals]]))

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

(defn find-or-create-model [client]
  (if-let [model (first (filter #(= "Swagger" (:name %)) (api/find-all (api/map->Model {}) client)))]
    model
    (-> (api/map->Model (parse-string (slurp (io/resource "model.json")) true))
        (api/create client))))

(defn- field-exists? [client field-name {:keys [_id] :as model}]
  (not (empty? (filter
                (fn [{:keys [name model]}]
                  (and (= name field-name)
                       (= model (str _id))))
                (api/find-all (api/map->Field {}) client)))))

(defn find-or-create-fields [client {model-id :_id :as model}]
  (when-not (field-exists? client "method" model)
    (-> (api/->Field "method" "method" "Text" (str model-id) [(api/type-id-by-name model "Operation")])
        (api/create client)))
  (when-not (field-exists? client "produces" model)
    (-> (api/->Field "produces" "produces" "List" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client)))
  (when-not (field-exists? client "consumes" model)
    (-> (api/->Field "consumes" "consumes" "List" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client))))

(defn model-template [m]
  (str "###JSON Schema\n```\n"
       (generate-string m {:pretty true})
       "\n```"))

(defn create-workspace [client url base-url title model {:keys [info] :as data}]
  (let [name (or title (:title info))]
    (-> (api/->Workspace name (tpl/render-resource "infoTemplate.tpl" (assoc info :workspaceName name :baseUrl base-url)) (str (:_id model)))
        (assoc :views ["swimlane" "sequence" "integrations" "componenttree" "relationships" "tableview" "tagscape" "reader" "processflow"])
        (api/create  client))))

(defn get-resource-listing [url]
  (let [{:keys [status body] :as resp} @(http/get (str (io/as-url url)) {:headers *custom-headers* :insecure? true})]
    (println "\nResponse from " url "\n")
    (clojure.pprint/pprint resp)
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

(defn save-models [models client]
  (map-vals #(let [schema (:schema %)]
               (assoc (api/create (dissoc % :schema) client) :schema schema)) models))

(defn update-comp [client component {:keys [produces consumes]}]
  (api/update 
   (cond-> (api/map->Component component)
     produces (assoc :produces produces)
     consumes (assoc :consumes consumes)) client))

(defn generate-operation-descripiton [data models]
  (reduce
   (fn [description [model-id {:keys [_id] :as model}]]
     (s/replace description (re-pattern (str "\\|" (name model-id) "\\|")) (str "|[" (name model-id) "](comp://" _id ")|")))
   (tpl/render-resource "operationTemplate.tpl" data)
   models))

(defn create-operations [client {wid :_id model-id :componentModel :as w} parent model models {:keys [path operations]}]
  (map
   (fn [{:keys [method summary notes type items parameters] :as data}]
     (-> (api/map->Component {:name (str method " " path)
                              :description (generate-operation-descripiton data models)
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
    (update-comp client component api-declaration)
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
  (mapcat
   (fn [model]
     (let [rrr (find-nested-model-deps (:schema model))]
       (keep
        (fn [model-key]
          (if-let [m ((keyword model-key) models)]
            (-> (api/map->Reference {:rootWorkspace (:rootWorkspace model)
                                     :source (str (:_id model))
                                     :target (str (:_id m))
                                     :type 3})
                (api/create client))))
        rrr)))
   (vals models)))

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

(defn import-swagger [client base-url name headers]
  (binding [*custom-headers* headers]
    (println "Importing swagger doc from " base-url ". Custom headers" *custom-headers*)
    (let [resource-listing (get-resource-listing base-url)
          url (resolve-url resource-listing base-url)
          model (find-or-create-model client)
          workspace (create-workspace client url base-url name model resource-listing)
          resources (doall (map (partial create-resource client workspace url model) (:apis resource-listing)))
          models (doall (-> (apply merge (map (partial create-models client url workspace model) resources))
                            (save-models client)))
          operations (doall (mapcat (partial create-api client url workspace model models) resources))
          refs (doall (create-refs client operations models))
          all {:workspace workspace
               :resources resources
               :models models
               :operations operations
               :refs refs}]
      (find-or-create-fields client model)
      (println "Done importing swagger doc from " base-url ".")
      (println "Imported " (count resources) " resources, " (count models) " json schemas," (count operations) " operations and " (count refs) " refs.")
      (str (:_id workspace)))))
