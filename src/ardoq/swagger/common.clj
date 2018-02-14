(ns ardoq.swagger.common
  (:require [ardoq.client :as api]
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
    (api/map->Model (get-model-template transformer-definition))
    (api/create client)))

(defn create-workspace-and-model [client wsname spec transformer-definition]
  ;; Creates a new workspace in the client.
  (let [model (create-model client transformer-definition)
        model-id (:_id model)
        workspace (->
                    (api/->Workspace wsname "" model-id)
                    (assoc
                      :views ["relationships" "tableview" "tagscape" "reader" "processflow"])
                    (api/create client))]
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

(defn- find-fields [client model-id]
  (seq (filter
         #(= model-id (:model %))
         (api/find-all (api/map->Field {}) client))))

(defn find-workspace-and-model [client wsname transformer-definition]
  (when-let [workspace (find-existing-resource client wsname #(api/map->Workspace {}))]
    (let [aggregated-workspace (api/find-aggregated workspace client)
          model-id (:componentModel workspace)
          model (-> {:_id model-id}
                  (api/map->Model)
                  (api/find-by-id client)
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
