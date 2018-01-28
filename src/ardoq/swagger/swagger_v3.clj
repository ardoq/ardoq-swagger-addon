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
  :OpenAPI-External-Documentation "OpenAPI External Documentation"})


(defn- string-object
  "Wraps a String in an Object that returns the given String when
  .toString is called. Wrapping a String like this prevents clostache
  seeing a String as a collection."
  [s]
  (when s
    (reify Object
      (toString [this] s))))

(defn render-resource
  "Wrapping Strings in object to stop Mustache from iterating over the string instead og simply rendering the string once"
  [template params]
  (tpl/render-resource template (clojure.walk/postwalk (fn [v] (if (string? v) (string-object v) v)) params)))


(defn transform-schema-object [schema-key parent-key data schema-object-spec]
  (if-let [ref (:$ref schema-object-spec)]
    (-> data
      (update-in [:references] conj {:source-key parent-key :target-path ref}))
    (let [key (str parent-key "/" (name schema-key))]
      (-> data
        (update-in [:swagger-object key] assoc :name (name schema-key))
        (update-in [:swagger-object key] assoc :type :OpenAPI-Schema)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (render-resource "templates/schema-object.tpl" schema-object-spec))))))


(defn transform-schemas [data spec parent-key]
  (let [key (str parent-key "/schemas")]
    (reduce
      (fn [data [schema-key schema-object-spec]]
        (transform-schema-object schema-key key data schema-object-spec))
      data
      (:schemas spec))))


(defn transform-parameter-object [parameter-name parent-key data parameter-object-spec]
  (if-let [ref (:$ref parameter-object-spec)]
    (-> data
      (update-in [:references] conj {:source-key parent-key :target-path ref}))
    (let [key (str parent-key "/" parameter-name)
          data (-> data
                 (update-in [:swagger-object key] assoc :name (:name parameter-object-spec))
                 (update-in [:swagger-object key] assoc :type :OpenAPI-Parameter)
                 (update-in [:swagger-object key] assoc :parent parent-key)
                 (update-in [:swagger-object key] assoc :description (render-resource "templates/parameter-object.tpl" parameter-object-spec)))]

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
      (update-in [:swagger-object key] assoc :description (render-resource "templates/operation-object.tpl" operation-object-spec))
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
               (update-in [:swagger-object key] assoc :description (render-resource "templates/path-object.tpl" path-object-spec))
               (transform-operation-objects path-object-spec key)
               (transform-parameter-objects path-object-spec key))]
    (name path-object-key)
    (if-let [ref (:$ref path-object-spec)]
      (-> data
        (update-in [:references] conj {:source-key key :target-path ref}))
      data)))

(defn transform-paths-object [data spec]
  (reduce
    transform-path-object
    data
    (:paths spec)))

(defn transform-components-object [data spec]
  (let [components-spec (:components spec)]
    (-> data
      (transform-schemas components-spec "#/components")
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
;;    :references [{:source-key {} :target-path "#/components/schema/someName"}]
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
          (prn "Syncing " spec-key parent-component-id)
          (assoc ardoq-components-by-spec-path spec-key synced-ardoq-component)))
    {}
    (:swagger-object spec-data)))


(defn clean-up-unused-components [client ardoq-data ardoq-sync-data]
  (let [ardoq-components (:key->component ardoq-data)
        orphan-keys (difference (set (keys ardoq-components)) (set (keys ardoq-sync-data)))
        orphans (select-keys ardoq-components orphan-keys)]

      #_(clojure.pprint/pprint (:key->reference ardoq-data))
    )

  )

(defn import-swagger3 [client spec wsname]
  (let [ardoq-data (or
                     (common/find-workspace-and-model client wsname :openapi-3.x)
                     (common/create-workspace-and-model client wsname spec :openapi-3.x))
        spec-data (transform-spec spec)
        ardoq-sync-data (sync-components client ardoq-data spec-data)
        deleted-components (clean-up-unused-components client ardoq-data ardoq-sync-data)]


    (prn "synced")
    #_(clojure.pprint/pprint ardoq-sync-data)

      ))



