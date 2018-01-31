(ns ardoq.swagger.swagger-v3
  (:require [ardoq.swagger.client :as api-client]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.model-utils :as model-utils]
            [ardoq.swagger.socket :refer [socket-send]]
            [cheshire.core :refer [generate-string parse-string]]
            [org.httpkit.server :as srv]
            [flatland.ordered.map :as maps]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [difference]]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]))



(def types {
  :OpenAPI-Structure "OpenAPI Structure"
  :OpenAPI-Server "OpenAPI Server"
  :OpenAPI-Operation "OpenAPI Operation"
  :OpenAPI-Path "OpenAPI Path"
  :OpenAPI-Security-Scheme "OpenAPI Security Scheme"
  :OpenAPI-Callback "OpenAPI Callback"
  :OpenAPI-Header "OpenAPI Header"
  :OpenAPI-Response "OpenAPI Response"
  :OpenAPI-Request-body "OpenAPI Request Body"
  :OpenAPI-Schema "OpenAPI Schema"
  :OpenAPI-Parameter "OpenAPI Parameter"
  :OpenAPI-Link "OpenAPI Link"
  :OpenAPI-Example "OpenAPI Example"
  :OpenAPI-Component "OpenAPI Component"
  :OpenAPI-Security-Requirement "OpenAPI Security Requirement"
  :OpenAPI-External-Documentation "OpenAPI External Documentation"
  :Orphan "Orphan"})


(def table-row-partial "|{{label}}|{{value}}|")

(defn render-resource-strings
  "Wrapping Strings in object to stop Mustache from iterating over the string instead og simply rendering the string once"
  ([template params]
    (render-resource-strings template params []))
  ([template params field-names]
    (let [fields (map (fn [[k v]] {:label (name k) :value (if (vector? v) (s/join ", " v) v)}) (select-keys params field-names))
          params (merge params {:fields fields
                                :hasFields (> (count fields) 0)})
          partials {:table-row table-row-partial}]
        (tpl/render-resource template params partials))))

#_(tpl/render-resource template (into {} (map (fn [[k v]] [k {:value (if (vector? v) (s/join ", " v) v)}]) params)))

(declare transform-schema-map transform-schema-list)

(def schema-table-fields
  [:format
   :title
   :default
   :multipleOf
   :maximum
   :exclusiveMaximum
   :minimum
   :exclusiveMinimum
   :maxLength
   :minLength
   :pattern
   :maxItems
   :minItems
   :uniqueItems
   :maxProperties
   :minProperties
   :required
   :enum
   :type
   :discriminator
   :readOnly
   :xml
   :externalDocs
   :example])

(defn transform-schema-object [schema-key parent-key data schema-object-spec]
  (let [key (str parent-key "/" (name schema-key))]
    (if-let [ref (:$ref schema-object-spec)]
      (-> data
        (update-in [:swagger-object key] assoc :name (str "$ref: " (:$ref schema-object-spec)))
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :type :OpenAPI-Schema)
        (update-in [:references] conj {:source-path key :target-path ref}))
      (-> data
        (update-in [:swagger-object key] assoc :name (name schema-key))
        (update-in [:swagger-object key] assoc :parent parent-key)
        (#(if (vector? schema-object-spec)
          (-> %
            (update-in [:swagger-object key] assoc :type :OpenAPI-Structure)
            (transform-schema-list schema-object-spec key))
          (-> %
            (update-in [:swagger-object key] assoc :type :OpenAPI-Schema)
            (update-in [:swagger-object key] assoc :description (render-resource-strings "templates/schema-object.tpl" schema-object-spec schema-table-fields))
            (transform-schema-map (select-keys schema-object-spec [:items :allOf :oneOf :anyOf :not :properties :additionalProperties]) key))))))))


(defn transform-schema-map [data schema-specs parent-key]
  (reduce
    (fn [data [schema-key schema-object-spec]]
      (transform-schema-object schema-key parent-key data schema-object-spec))
    data
    schema-specs))


(defn transform-schema-list [data schema-list parent-key]
  (reduce
    (fn [data schema-object-spec]
      (clojure.pprint/pprint schema-object-spec)
      (let [key (or (:name schema-object-spec) (s/join ", " (:required schema-object-spec)) (:type schema-object-spec) "schema") ]
        (transform-schema-object key parent-key data schema-object-spec)))
    data
    schema-list))


(def paramter-table-fields
  [:in
   :required
   :deprecated
   :allowEmptyValue
   :style
   :explode
   :allowReserved
   :example])

(defn transform-parameter-object [parameter-name parent-key data parameter-object-spec]
  (if-let [ref (:$ref parameter-object-spec)]
    (-> data
      (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" parameter-name)
          data (-> data
                   (update-in [:swagger-object key] assoc :name (:name parameter-object-spec))
                   (update-in [:swagger-object key] assoc :type :OpenAPI-Parameter)
                   (update-in [:swagger-object key] assoc :parent parent-key)
                   (update-in [:swagger-object key] assoc :description (render-resource-strings "templates/parameter-object.tpl" parameter-object-spec paramter-table-fields)))]

      (if-let [schema-object-spec (:schema parameter-object-spec)]
        (transform-schema-object :schema key data (:schema parameter-object-spec))
        data))))


(defn transform-parameter-objects [data spec parent-key]
  (reduce
    (fn [data parameter-object-spec]
      (if (map-entry? parameter-object-spec)
        (transform-parameter-object (key parameter-object-spec) parent-key data (val parameter-object-spec))
        (transform-parameter-object (:name parameter-object-spec) parent-key data parameter-object-spec)))
    data
    (:parameters spec)))


(defn transform-operation-object [parent-key data [operation-object-key operation-object-spec]]
  (let [key (str parent-key "/" (name operation-object-key))]
    (-> data
      (update-in [:swagger-object key] assoc :name (name operation-object-key))
      (update-in [:swagger-object key] assoc :type :OpenAPI-Operation)
      (update-in [:swagger-object key] assoc :parent parent-key)
      (update-in [:swagger-object key] assoc :description (render-resource-strings "templates/operation-object.tpl" operation-object-spec [:tags :operationId]))
      (transform-parameter-objects operation-object-spec key))))


(defn transform-operation-objects [data path-spec parent-key]
  (reduce
    (partial transform-operation-object parent-key)
    data
    (select-keys path-spec [:get :put :post :delete :options :head :patch :trace])))


(defn transform-path-object [data [path-object-key path-object-spec]]
  (let [parent-key "#/paths"
        key (str parent-key "/" (name path-object-key))
        data (-> data
               (update-in [:swagger-object key] assoc :name (subs (str path-object-key) 1))
               (update-in [:swagger-object key] assoc :type :OpenAPI-Path)
               (update-in [:swagger-object key] assoc :parent parent-key)
               (update-in [:swagger-object key] assoc :description (render-resource-strings "templates/path-object.tpl" path-object-spec))
               (transform-operation-objects path-object-spec key)
               (transform-parameter-objects path-object-spec key))]
    (name path-object-key)
    (if-let [ref (:$ref path-object-spec)]
      (-> data
        (update-in [:references] conj {:source-path key :target-path ref}))
      data)))

(defn transform-paths-object [data spec]
  (reduce
    transform-path-object
    data
    (:paths spec)))

(defn transform-components-object [data spec]
  (let [components-spec (:components spec)]
    (-> data
      (transform-schema-map (:schemas components-spec) "#/components/schemas")
      (transform-parameter-objects components-spec "#/components/parameters")
      )))

(defn create-scaffolding [data]
  (-> data
    (assoc-in [:swagger-object "#/components"] {:name "Components" :type :OpenAPI-Structure})
    (assoc-in [:swagger-object "#/components/schemas"] {:name "Schemas" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/responses"] {:name "Responses" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/parameters"] {:name "Parameters" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/examples"] {:name "Examples" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/requestBodies"] {:name "Request Bodies" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/headers"] {:name "Headers" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/securitySchemes"] {:name "Security Schemes" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/links"] {:name "Links" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/components/callbacks"] {:name "Callbacks" :type :OpenAPI-Structure :parent "#/components"})
    (assoc-in [:swagger-object "#/paths"] {:name "Paths" :type :OpenAPI-Structure})))


(defn transform-spec [spec]
;;   {:key->swagger-object {{:name "n" :type "typename"} {:name "n" :description "descr"}}
;;    :references [{:source-path {} :target-path "#/components/schema/someName"}]
;;    :spec-path->key {"#/components/schema/someName" {:name "n" :type "typename"}}}

  (let [data {:swagger-object (maps/ordered-map)
              :references []
              :spec-path-to-key-map {}}]
    (->
      data
      (create-scaffolding)
      (transform-paths-object spec)
      (transform-components-object spec))))


(defn sync-component [client ardoq-data parent-component-id [spec-key spec-data-item]]
  (if-let [existing-component (get-in ardoq-data [:key->component spec-key])]
    (let [updated-component (-> existing-component
                              (merge (select-keys spec-data-item [:name :description]))
                              (assoc :parent parent-component-id)
                              (assoc :open-api-path spec-key))]
      (if (= updated-component existing-component)
        (api-client/map->Component existing-component)
        (api-client/update (api-client/map->Component updated-component) client)))
    (let [new-component (->
                          spec-data-item
                          (select-keys [:name :description])
                          (assoc :typeId (name (get-in ardoq-data [:model-name->type-id (types (:type spec-data-item))])))
                          (assoc :parent parent-component-id)
                          (assoc :open-api-path spec-key)
                          (assoc :rootWorkspace (get-in ardoq-data [:workspace :_id]))
                          (assoc :model (get-in ardoq-data [:model :_id])))]
      (api-client/create (api-client/map->Component new-component) client))))


(defn sync-components [client ardoq-data spec-data]
  (reduce
    (fn [ardoq-components-by-spec-path [spec-key spec-data-item]]
      (let [parent-component-id (some->
                                  spec-data-item
                                  :parent
                                  ardoq-components-by-spec-path
                                  :_id)
            synced-ardoq-component
              (sync-component client ardoq-data parent-component-id [spec-key spec-data-item])]
          (prn "Syncing " spec-key (:_id synced-ardoq-component))
          (assoc ardoq-components-by-spec-path spec-key synced-ardoq-component)))
    {}
    (:swagger-object spec-data)))


(defn find-and-categorise-orphan-components [client ardoq-data ardoq-sync-data]
  (let [ardoq-components (:key->component ardoq-data)
        orphan-keys (difference (set (keys ardoq-components)) (set (keys ardoq-sync-data)))
        orphan-components (vals (select-keys ardoq-components orphan-keys))
        components-referencing-other-workspaces (:components-referencing-other-workspaces ardoq-data)]

    (group-by
      (fn [comp]
        (if
          (contains? components-referencing-other-workspaces (:_id comp))
          :to-mark-as-orphan
          :to-delete))
      orphan-components)))


(defn delete-components [client components]
  (doall
    (map
      (fn [orphan-component]
        (prn "deleting " (:name orphan-component) (:_id orphan-component))
        (try
          (api-client/delete (api-client/map->Component orphan-component) client)
          ;; is it better to delete from bottom and up?
          (catch Exception e (str "Exception deleting component. Probably because a parent was deleted before a child"))))
      components)))


(defn mark-as-orphans [client ardoq-data components]
  (doall
    (map
      (fn [orphan-component]
        (prn "changing type of " (:name orphan-component) (:_id orphan-component))
        (let [orphan-type (name (get-in ardoq-data [:model-name->type-id (types :Orphan)]))]
          (-> orphan-component
            (assoc :description (render-resource-strings "templates/orphan-object.tpl" orphan-component))
            (assoc :parent nil)
            (assoc :open-api-path nil)
            (assoc :typeId orphan-type)
            (api-client/map->Component)
            (api-client/update client))))
      components)))


(defn map-to-ardoq-ids [ardoq-sync-components spec-data]
  (->>
    (:references spec-data)
    (map
      (fn [{source-path :source-path target-path :target-path }]
        {:source (:_id (ardoq-sync-components source-path))
         :target (:_id (ardoq-sync-components target-path))}))
    (filter
      (fn [{source :source target :target}]
        (and source target)))
    (into #{})))


(defn create-reference [client ardoq-data {source :source target :target}]
  (prn "Creating reference from" source "to" target)
  (-> {
        :source source
        :target target
        :type 0
        :rootWorkspace (get-in ardoq-data [:workspace :_id])
        :targetWorkspace (get-in ardoq-data [:workspace :_id])}
     (api-client/map->Reference)
     (api-client/create client)
     ))


(defn delete-reference [client ardoq-data ref-key]
  (prn "Deleting reference from" (:source ref-key) "to" (:target ref-key))

  (prn (get-in ardoq-data [:key->reference ref-key]))

  (-> (get-in ardoq-data [:key->reference ref-key])
    (api-client/map->Reference)
    (api-client/delete client)))


(defn sync-references [client ardoq-data ardoq-sync-components spec-data]
  (let [spec-refs (map-to-ardoq-ids ardoq-sync-components spec-data)
        current-refs (set (keys (:key->reference ardoq-data)))
        new-refs (difference spec-refs current-refs)
        superfluous-refs (difference current-refs spec-refs)]

    (doall (map (partial create-reference client ardoq-data) new-refs))
    (doall (map (partial delete-reference client ardoq-data) superfluous-refs))))


(defn import-swagger3 [client spec wsname]
  (let [ardoq-data (or
                     (common/find-workspace-and-model client wsname :openapi-3.x)
                     (common/create-workspace-and-model client wsname spec :openapi-3.x))
        spec-data (transform-spec spec)
        ardoq-sync-components (sync-components client ardoq-data spec-data)
        orphan-components (find-and-categorise-orphan-components client ardoq-data ardoq-sync-components)]

    (delete-components client (:to-delete orphan-components))
    (mark-as-orphans client ardoq-data (:to-mark-as-orphan orphan-components))
    (sync-references client ardoq-data ardoq-sync-components spec-data)

    (prn (set (keys (:key->reference ardoq-data))))


    (prn "synced")

  ))



