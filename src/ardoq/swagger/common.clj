(ns ardoq.swagger.common
  (:require [ardoq.client :as api-client]
            [ardoq.swagger.model-utils :as model-utils]
            [ardoq.swagger.socket :refer [socket-send]]
            [cheshire.core :refer [generate-string parse-string]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojurewerkz.urly.core :as urly]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]
            [clj-http.client :as http]))

(defn replace-html-tags [schema]
  (-> schema
      (s/replace #"\\n" "\n")
      (s/replace #"</*b>" "**")
      (s/replace #"<br/*>" "\n")
      (s/replace #"<a href='(.+)'>(.+)</a>" "[$2]($1)")
      (s/replace #"</*[a-z]+/*>" "")))


(def table-row-partial "|{{{label}}}|{{{value}}}|")

(defn table-cell-str [v]
  (cond
    (vector? v) (s/join ", " v)
    (map? v) (s/join ", " v) ;;TODO: print map as yaml
    :else v))

(defn render-resource-strings
  "Wrapping Strings in object to stop Mustache from iterating over the string instead og simply rendering the string once"
  ([template params]
   (render-resource-strings template params []))
  ([template params field-names]
   (let [fields (map (fn [[k v]] {:label (name k) :value (table-cell-str v)}) (select-keys params field-names))
         params (merge params {:fields fields
                               :hasFields (> (count fields) 0)})
         partials {:table-row table-row-partial}]
     (tpl/render-resource template params partials))))


(defn get-model-template [transformer-definition]
  (let [resource (:model-file transformer-definition)]
    (parse-string (slurp (io/resource resource)) true)))

(defn create-model [client transformer-definition]
  (->
    (api-client/map->Model (get-model-template transformer-definition))
    (api-client/create client)))

(defn create-workspace-and-model [client wsname spec transformer-definition]
  ;; Creates a new workspace in the client.
  (let [model (create-model client transformer-definition)
        model-id (:_id model)
        workspace (->
                    (api-client/->Workspace wsname "" model-id)
                    (assoc
                      :views ["relationships" "tableview" "tagscape" "reader" "processflow"])
                    (api-client/create client))]
    {:new? true
     :model model
     :model-name->type-id (model-utils/type-ids-by-name model)
     :key->component {}
     :key-reference {}
     :workspace workspace}))


(defn ensure-model-has-all-types [model client transformer-definition]
  (let [model-template (get-model-template transformer-definition)]
;;        name->model-type (model-utils/type-ids-by-name model)
;;        name->template-type (model-utils/type-ids-by-name model-template)]


    model)
  )

(defn create-reference [client workspace-id type-id {source :source target :target}]
  (socket-send (str "Creating reference from" source "to" target))
  (->
    {:source          source
     :target          target
     :type            type-id
     :rootWorkspace   workspace-id
     :targetWorkspace workspace-id}
    (api-client/map->Reference)
    (api-client/create client)))


(defn delete-reference [client ardoq-data ref-key]
  (socket-send (str "Deleting reference from" (:source ref-key) "to" (:target ref-key)))

  (->
    (get-in ardoq-data [:key->reference ref-key])
    (api-client/map->Reference)
    (api-client/delete client)))


(defn create-component [client ardoq-data parent-component-id [spec-key spec-data-item] transformer-definition]
  (socket-send (str "Creating ardoq component for " spec-key))
  (let [type-key (:type spec-data-item)
        ardoq-type-name (get-in transformer-definition [:model-types type-key])
        ardoq-type-id (name (get-in ardoq-data [:model-name->type-id ardoq-type-name]))]
    (->
      spec-data-item
      (select-keys [:name :description])
      (assoc :typeId ardoq-type-id)
      (assoc :parent parent-component-id)
      (assoc :open-api-path spec-key)
      (assoc :rootWorkspace (get-in ardoq-data [:workspace :_id]))
      (assoc :model (get-in ardoq-data [:model :_id]))
      (api-client/map->Component)
      (api-client/create  client))))


(defn update-component [client existing-component parent-component-id [spec-key spec-data-item]]
  (socket-send (str "Updating ardoq component for " spec-key))
  (let [updated-component (-> existing-component
                              (merge (select-keys spec-data-item [:name :description]))
                              (assoc :parent parent-component-id)
                              (assoc :open-api-path spec-key))]
    (if (= updated-component existing-component)
      (api-client/map->Component existing-component)
      (api-client/update (api-client/map->Component updated-component) client))))


(defn find-existing-resource 
  ([client name type]
   (first (filter #(= name (:name %)) (api-client/find-all (type) client))))
  ([client name type root-id]
   (first (filter #(= name (:name %)) (api-client/find-in-workspace (type) client root-id)))))


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

(defn- find-fields [client model-id]
  (seq (filter
         #(= model-id (:model %))
         (api-client/find-all (api-client/map->Field {}) client))))

(defn find-workspace-and-model [client wsname transformer-definition]
  (when-let [workspace (find-existing-resource client wsname #(api-client/map->Workspace {}))]
    (let [aggregated-workspace (api-client/find-aggregated workspace client)
          model-id (:componentModel workspace)
          model (-> {:_id model-id}
                  (api-client/map->Model)
                  (api-client/find-by-id client)
                  (ensure-model-has-all-types client transformer-definition))
          model-types-by-id (model-utils/to-component-type-map model)]

      {:new? false
       :model model
       :fields (find-fields client model-id)
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
         (api-client/find-all (api-client/map->Field {}) client))))

(defn find-or-create-fields [client {model-id :_id :as model}]
  (when-not (field-exists? client "method" model)
    (-> (api-client/->Field "method" "method" "Text" (str model-id) [(model-utils/type-id-by-name model "Operation")])
        (api-client/create client)))
  (when-not (field-exists? client "produces" model)
    (-> (api-client/->Field "produces" "produces" "Text" (str model-id) [(model-utils/type-id-by-name model "Operation") (model-utils/type-id-by-name model "Resource")])
        (api-client/create client)))
  (when-not (field-exists? client "consumes" model)
    (-> (api-client/->Field "consumes" "consumes" "Text" (str model-id) [(model-utils/type-id-by-name model "Operation") (model-utils/type-id-by-name model "Resource")])
        (api-client/create client))))
