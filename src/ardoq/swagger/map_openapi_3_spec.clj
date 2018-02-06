(ns ardoq.swagger.map-openapi-3-spec
  (:require [ardoq.swagger.client :as api-client]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.model-utils :as model-utils]
            [ardoq.swagger.socket :refer [socket-send]]
            [cheshire.core :refer [generate-string parse-string]]
            [flatland.ordered.map :as maps]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [difference]]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]))

(def model-file "model-openapi-3.x.json")

(def model-types {
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



#_(tpl/render-resource template (into {} (map (fn [[k v]] [k {:value (if (vector? v) (s/join ", " v) v)}]) params)))

(defn transform-map [transform-fn data spec-map parent-key]
  (reduce
    (fn [data [spec-key spec-object]]
      (apply transform-fn [spec-key parent-key data spec-object]))
    data
    spec-map))


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
                  (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/schema-object.tpl" schema-object-spec schema-table-fields))
                  (transform-schema-map (select-keys schema-object-spec [:items :allOf :oneOf :anyOf :not :properties :additionalProperties]) key))))))))


(defn transform-schema-map [data schema-specs parent-key]
  (transform-map transform-schema-object data schema-specs parent-key))


(defn transform-response-object [response-key parent-key data response-object-spec]
  (let [key (str parent-key "/" (name response-key))]
    (if-let [ref (:$ref response-object-spec)]
      (-> data
          (update-in [:swagger-object key] assoc :name (str "$ref: " (:$ref response-object-spec)))
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :type :OpenAPI-Response)
          (update-in [:references] conj {:source-path key :target-path ref}))
      (-> data
          (update-in [:swagger-object key] assoc :name (name response-key))
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :type :OpenAPI-Response)
          (update-in [:swagger-object key] assoc :description (:description response-object-spec))
          ))))


(defn transform-responses-map [data response-specs parent-key]
  (transform-map transform-response-object data response-specs parent-key))


(defn transform-schema-list [data schema-list parent-key]
  (reduce
    (fn [data schema-object-spec]
      (let [key (or (:name schema-object-spec) (s/join ", " (:required schema-object-spec)) (:type schema-object-spec) "schema") ]
        (transform-schema-object key parent-key data schema-object-spec)))
    data
    schema-list))


(def parameter-table-fields
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
                   (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/parameter-object.tpl" parameter-object-spec parameter-table-fields)))]

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
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/operation-object.tpl" operation-object-spec [:tags :operationId]))
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
                 (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/path-object.tpl" path-object-spec))
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
        (transform-responses-map (:responses components-spec) "#/components/responses")
        (transform-parameter-objects components-spec "#/components/parameters")

        )))

(def server-variable-fields [:enum :default])

(defn transform-server-variable [var-name var-spec]
  (let [var-spec (assoc var-spec :variableName (name var-name))]
    (common/render-resource-strings "templates/server-variable-object.tpl" var-spec server-variable-fields)))

(defn transform-server-variables [variables-spec]
  (reduce
    (fn [acc [var-name var-spec]]
      (let [variable-md (transform-server-variable var-name var-spec)]
        (str acc variable-md)))
    ""
    variables-spec))

(defn transform-server-object [data server-object-spec]
  (let [key (:url server-object-spec)
        parent-key "#/servers"
        render-data {:description (:description server-object-spec)
                     :variables (transform-server-variables (:variables server-object-spec))}]

    (-> data
        (update-in [:swagger-object key] assoc :name key)
        (update-in [:swagger-object key] assoc :type :OpenAPI-Server)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/server-object.tpl" render-data)))))

(defn transform-servers-object [data spec]
  (reduce
    transform-server-object
    data
    (:servers spec)))

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
      (assoc-in [:swagger-object "#/paths"] {:name "Paths" :type :OpenAPI-Structure})
      (assoc-in [:swagger-object "#/servers"] {:name "Servers" :type :OpenAPI-Structure})))


(def info-object-fields
  [:title
   :version
   :termsOfService
   :contact.name
   :contact.email
   :contact.url
   :license.name
   :license.url])

(def contact-object-fields
  [:name :email :url])

(def license-object-fields
  [:name :url])

(defn workspace-description [spec]
  (let [info-spec (:info spec)
        info-spec (assoc info-spec :importTime (.format (java.text.SimpleDateFormat. "yyyy.MM.dd HH:mm") (new java.util.Date)))
        contact-object-md (common/render-resource-strings "templates/contact-object.tpl" (:contact info-spec) contact-object-fields)
        info-spec (assoc info-spec :contact contact-object-md)
        license-object-md (common/render-resource-strings "templates/license-object.tpl" (:license info-spec) license-object-fields)
        info-spec (assoc info-spec :license license-object-md)]
    (common/render-resource-strings "templates/info-object.tpl" info-spec info-object-fields)))

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
      (transform-servers-object spec)
      (transform-components-object spec))))

(def transformer-definition
  {:model-file model-file
   :model-types model-types
   :workspace-description-fn workspace-description
   :transform-spec-fn transform-spec})