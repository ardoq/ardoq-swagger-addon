(ns ardoq.swagger.common
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.model-utils :as model-utils]
            [cheshire.core :refer [generate-string parse-string]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojurewerkz.urly.core :as urly]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]
            [clj-http.client :as http]))

(defn replace-html-tags [schema]
  (-> schema
      (clojure.string/replace #"\\n" "\n")
      (clojure.string/replace #"</*b>" "**")
      (clojure.string/replace #"<br/*>" "\n")
      (clojure.string/replace #"<a href='(.+)'>(.+)</a>" "[$2]($1)")
      (clojure.string/replace #"</*[a-z]+/*>" "")))


(defn get-model-template [spec-version]
  (let [resource (case spec-version
                   :swagger-1.x "modelv1.json"
                   :swagger-2.x "modelv2.json"
                   :openapi-3.x "modelv3.json")]
    (parse-string (slurp (io/resource resource)) true)))

(defn create-model [client spec-version]
  (->
    (api/map->Model (get-model-template spec-version))
    (api/create client)))

(defn create-workspace-and-model [client wsname spec spec-version]
  ;; Creates a new workspace in the client.
  (let [model (create-model client spec-version)
        model-id (:_id model)
        description (tpl/render-resource "templates/infoTemplate.tpl" spec)
        workspace (->
                    (api/->Workspace wsname description model-id)
                    (assoc
                      :views ["relationships" "tableview" "tagscape" "reader" "processflow"])
                    (api/create client))]
    {:new? true
     :model model
     :model-name->type-id (model-utils/type-ids-by-name model)
     :key->component {}
     :key-reference {}
     :workspace workspace}))

(defn ensure-model-has-all-types [model client spec-version]
  (let [model-template (get-model-template spec-version)])
  ;;TODO actually do something here
  model)


(defn find-existing-resource 
  ([client name type]
   (first (filter #(= name (:name %)) (api/find-all (type) client))))
  ([client name type root-id]
   (first (filter #(= name (:name %)) (api/find-in-workspace (type) client root-id)))))


(defn find-components-referencing-other-workspaces [aggregated-workspace]
  (into #{}
    (concat
      (map :target (:incoming-references aggregated-workspace))
      (->>
        (:references aggregated-workspace)
        (filter #(not= (:rootWorkspace %) (:targetWorkspace %)))
        (map :source)
        ))))

(defn map-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. Only the last element with a key will remain."
  [f coll]
  (persistent!
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc! ret k x)))
      (transient {}) coll)))

(defn find-workspace-and-model [client wsname spec-version]
  (when-let [workspace (find-existing-resource client wsname #(api/map->Workspace {}))]
    (let [aggregated-workspace (api/find-aggregated workspace client)
          model-id (:componentModel workspace)
          model (-> {:_id model-id}
                  (api/map->Model)
                  (api/find-by-id client)
                  (ensure-model-has-all-types client spec-version))
          model-types-by-id (model-utils/to-component-type-map model)]

      {:new? false
       :model model
       :model-name->type-id (model-utils/type-ids-by-name model)
       :key->component (->> (:components aggregated-workspace) (filter :open-api-path) (map-by :open-api-path))
;;       :id->component (reduce #(assoc %1 (:_id %2) %2) {} (:components aggregated-workspace))
       :key->reference (->> (:references aggregated-workspace) (map-by #(select-keys % [:source :target])))
       :components-referencing-other-workspaces (find-components-referencing-other-workspaces aggregated-workspace)
       :workspace workspace})))


(defn- field-exists? [client field-name {:keys [_id] :as model}]
  (seq (filter
        (fn [{:keys [name model]}]
          (and (= name field-name)
               (= model (str _id))))
        (api/find-all (api/map->Field {}) client))))

(defn find-or-create-fields [client {model-id :_id :as model}]
  (when-not (field-exists? client "method" model)
    (-> (api/->Field "method" "method" "Text" (str model-id) [(model-utils/type-id-by-name model "Operation")])
        (api/create client)))
  (when-not (field-exists? client "produces" model)
    (-> (api/->Field "produces" "produces" "Text" (str model-id) [(model-utils/type-id-by-name model "Operation") (model-utils/type-id-by-name model "Resource")])
        (api/create client)))
  (when-not (field-exists? client "consumes" model)
    (-> (api/->Field "consumes" "consumes" "Text" (str model-id) [(model-utils/type-id-by-name model "Operation") (model-utils/type-id-by-name model "Resource")])
        (api/create client))))
