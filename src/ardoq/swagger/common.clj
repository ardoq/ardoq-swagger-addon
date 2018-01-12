(ns ardoq.swagger.common
  (:require [ardoq.swagger.client :as api]
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

(defn model-template [m]
  (str "###JSON Schema\n```json\n"
       (generate-string m {:pretty true})
       "\n```"))

(defn generate-param-description [data]
  (-> (tpl/render-resource "globalTemplate.tpl" data)))

(defn generate-security-description[data]
  (tpl/render-resource "securityTemplate.tpl" data))

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
        description (tpl/render-resource "infoTemplate.tpl" spec)
        workspace (->
                    (api/->Workspace wsname description model-id)
                    (assoc
                      :views ["relationships" "tableview" "tagscape" "reader" "processflow"])
                    (api/create client))]
    {:new? true
     :model model
     :key->component {}
     :references {}
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

(defn find-workspace-and-model [client wsname spec-version]
  (when-let [workspace (find-existing-resource client wsname #(api/map->Workspace {}))]
    (let [aggregated-workspace (api/find-aggregated workspace client)
          model-id (:componentModel workspace)
          model (-> {:_id model-id}
                  (api/map->Model)
                  (api/find-by-id client)
                  (ensure-model-has-all-types client spec-version))]

      {:new? false?
       :model model
       :key->component (:components aggregated-workspace)
       :references (:references aggregated-workspace)
       :workspace workspace})))


(defn- field-exists? [client field-name {:keys [_id] :as model}]
  (seq (filter
        (fn [{:keys [name model]}]
          (and (= name field-name)
               (= model (str _id))))
        (api/find-all (api/map->Field {}) client))))

(defn find-or-create-fields [client {model-id :_id :as model}]
  (when-not (field-exists? client "method" model)
    (-> (api/->Field "method" "method" "Text" (str model-id) [(api/type-id-by-name model "Operation")])
        (api/create client)))
  (when-not (field-exists? client "produces" model)
    (-> (api/->Field "produces" "produces" "Text" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client)))
  (when-not (field-exists? client "consumes" model)
    (-> (api/->Field "consumes" "consumes" "Text" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client))))

(defn generate-operation-description [data models]
  (let [data (-> data 
                 (replace-html-tags)
                 (read-string))]
    (reduce
     (fn [description [model-id {:keys [_id] :as model}]]
       (s/replace description (re-pattern (str "\\|" (name model-id) "\\|")) (str "|[" (name model-id) "](comp://" _id ")|")))
     (tpl/render-resource "operationTemplate.tpl" data)
     models)))

(defn update-comp [client component {:keys [produces consumes]}]
  ;; Updates a component based on previous modelling. Uses the swagger file to detect what it needs. 
  (api/update 
   (cond-> (api/map->Component component)
           produces (-> ;This gets the cond macro
                     (assoc :produces produces)
                     (dissoc :consumes))
           consumes (-> ;This gets the cond macro
                     (assoc :consumes consumes)
                     (dissoc :produces))) client))

(defn save-models [models client workspace]
  (map-vals (fn [model] 
              (let [schema (:schema model)]
                (if (first (filter #(and (= (:type %) "Model") (= (:name %) (:name model))) (:components workspace)))
                  (assoc (api/update (dissoc model :schema) client) :schema schema)
                  (assoc (api/create (dissoc model :schema) client) :schema schema)))) 
            models))


(defn find-or-create-tag [client tag wid op tags]
  (doall (map (fn [name]
                ;;Check if the tag exists, otherwise we create it
                (if (not (clojure.string/blank? name))
                  (if (get (deref tags) name)
                    (when-not (some #{(:_id op)} (get-in @tags [name :components]))
                      (swap! tags (fn [old]
                                    (update-in old [name :components] conj (get-in op [:_id])))))
                    (swap! tags (fn [old]
                                  (assoc old name (api/->Tag name "" wid [(get-in op [:_id])] [])))))))
              tag)))
