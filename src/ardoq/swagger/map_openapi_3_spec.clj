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
            :OpenAPI-Media-Type "OpenAPI Media Type"
            :OpenAPI-Request-body "OpenAPI Request Body"
            :OpenAPI-Schema "OpenAPI Schema"
            :OpenAPI-Parameter "OpenAPI Parameter"
            :OpenAPI-Link "OpenAPI Link"
            :OpenAPI-Example "OpenAPI Example"
            :OpenAPI-Component "OpenAPI Component"
            :OpenAPI-Security-Requirement "OpenAPI Security Requirement"
            :OpenAPI-External-Documentation "OpenAPI External Documentation"
            :Orphan "Orphan"})


(defn transform-objects [data transform-object-fn param-spec parent-key spec-type]
  (reduce
    (fn [data object-spec]
      (if (map-entry? object-spec)
        (apply transform-object-fn [(name (key object-spec)) parent-key data (val object-spec) spec-type])
        (apply transform-object-fn [(or (:name object-spec) (name spec-type)) parent-key data object-spec spec-type])))
    data
    param-spec))


(declare transform-schema-list transform-parameter-object transform-server-object)

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

(defn transform-schema-object [schema-key parent-key data schema-object-spec spec-type]
  (let [key (str parent-key "/" (name schema-key))]
    (if-let [ref (:$ref schema-object-spec)]
      (-> data
          (update-in [:swagger-object key] assoc :name (str "$ref: " (:$ref schema-object-spec)))
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :type spec-type)
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
                  (transform-objects transform-schema-object (select-keys schema-object-spec [:items :allOf :oneOf :anyOf :not :properties :additionalProperties]) key :OpenAPI-Schema))))))))


(defn transform-schema-list [data schema-list parent-key]
  (reduce
    (fn [data schema-object-spec]
      (let [key (or (:name schema-object-spec) (s/join ", " (:required schema-object-spec)) (:type schema-object-spec) "schema") ]
        (transform-schema-object key parent-key data schema-object-spec :OpenAPI-Schema)))
    data
    schema-list))


(defn transform-example-object [parameter-name parent-key data example-object-spec spec-type]
  (if-let [ref (:$ref example-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" parameter-name)
          example-object-spec (assoc example-object-spec :hasValue (some? (:value example-object-spec)))
          example-object-spec (assoc example-object-spec :hasExternalValue (some? (:externalValue example-object-spec)))]
      (-> data
          (update-in [:swagger-object key] assoc :name parameter-name)
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/example-object.tpl" example-object-spec []))))))


(def encoding-table-fields
  [:contentType
   :style
   :explode
   :allowReserved])

(defn transform-encoding-object [parameter-name parent-key data encoding-object-spec spec-type]
  (let [key (str parent-key "/" parameter-name)]
    (-> data
        (update-in [:swagger-object key] assoc :name parameter-name)
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/encoding-object.tpl" encoding-object-spec encoding-table-fields))
        (transform-objects transform-parameter-object (:headers encoding-object-spec) key :OpenAPI-Header))))

(def link-table-fields
  [:operationRef
   :operationId
   :requestBody])

(defn transform-link-object [parameter-name parent-key data link-object-spec spec-type]
  (if-let [ref (:$ref link-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" parameter-name)
          link-parameters (:parameters link-object-spec)
          link-parameters-md (common/render-resource-strings "templates/link-parameters.tpl" link-parameters (keys link-parameters))
          link-object-spec (assoc link-object-spec :linkParameters link-parameters-md)]
      (-> data
          (update-in [:swagger-object key] assoc :name parameter-name)
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/link-object.tpl" link-object-spec link-table-fields))
          (#(if-let [server-object-spec (:server link-object-spec)]
              (transform-server-object % server-object-spec)
              %))))))

(defn transform-media-type-object [parameter-name parent-key data media-type-object-spec spec-type]
  (let [key (str parent-key "/" parameter-name)]
    (-> data
        (update-in [:swagger-object key] assoc :name parameter-name)
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/media-type-object.tpl" media-type-object-spec []))
        (#(if-let [schema-object-spec (:schema media-type-object-spec)]
            (transform-schema-object :schema key % schema-object-spec :OpenAPI-Schema)
            %))
        (transform-objects transform-encoding-object (:encoding media-type-object-spec) key :OpenAPI-Encoding)
        (transform-objects transform-example-object (:examples media-type-object-spec) key :OpenAPI-Example))))


(def request-body-fields
  [:required])

(defn transform-request-body-object [parameter-name parent-key data request-body-object-spec spec-type]
  (if-let [ref (:$ref request-body-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" parameter-name)]
      (-> data
          (update-in [:swagger-object key] assoc :name parameter-name)
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/request-body-object.tpl" request-body-object-spec request-body-fields))
          (transform-objects transform-media-type-object (:content request-body-object-spec) key :OpenAPI-Media-Type)))))


(def parameter-table-fields
  [:in
   :required
   :deprecated
   :allowEmptyValue
   :style
   :explode
   :allowReserved
   :example])


(defn transform-parameter-object [parameter-name parent-key data parameter-object-spec spec-type]
  (if-let [ref (:$ref parameter-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" parameter-name)]
      (-> data
          (update-in [:swagger-object key] assoc :name parameter-name)
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/parameter-object.tpl" parameter-object-spec parameter-table-fields))
          (transform-objects transform-media-type-object (:content parameter-object-spec) key :OpenAPI-Media-Type)
          (transform-objects transform-example-object (:examples parameter-object-spec) key :OpenAPI-Example)
          (#(if-let [schema-object-spec (:schema parameter-object-spec)]
              (transform-schema-object :schema key % schema-object-spec :OpenAPI-Schema)
              %))))))


(defn transform-response-object [response-key parent-key data response-object-spec spec-type]
  (let [key (str parent-key "/" (name response-key))]
    (if-let [ref (:$ref response-object-spec)]
      (-> data
          (update-in [:swagger-object key] assoc :name (str "$ref: " (:$ref response-object-spec)))
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:references] conj {:source-path key :target-path ref}))
      (-> data
          (update-in [:swagger-object key] assoc :name (name response-key))
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :description (:description response-object-spec))
          (transform-objects transform-parameter-object (:headers response-object-spec) key :OpenAPI-Header)
          (transform-objects transform-media-type-object (:content response-object-spec) key :OpenAPI-Media-Type)
          (transform-objects transform-link-object (:links response-object-spec) key :OpenAPI-Link)))))


(def operation-fields
  [:tags
   :operationId
   :deprecated])

(defn transform-operation-object [parent-key data [operation-object-key operation-object-spec]]
  (let [key (str parent-key "/" (name operation-object-key))]
    (-> data
        (update-in [:swagger-object key] assoc :name (name operation-object-key))
        (update-in [:swagger-object key] assoc :type :OpenAPI-Operation)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/operation-object.tpl" operation-object-spec operation-fields))
        (transform-objects transform-parameter-object (:parameters operation-object-spec) key :OpenAPI-Parameter)
        (#(if-let [request-body-spec (:requestBody operation-object-spec)]
            (transform-request-body-object :requestBody key % request-body-spec :OpenAPI-Request-body)
            %))
        (transform-objects transform-response-object (:responses operation-object-spec) key :OpenAPI-Response))))


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
                 (transform-objects transform-parameter-object (:parameters path-object-spec) key :OpenAPI-Parameter))]
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
        (transform-objects transform-schema-object (:schemas components-spec) "#/components/schemas" :OpenAPI-Schema)
        (transform-objects transform-response-object (:responses components-spec) "#/components/responses" :OpenAPI-Response)
        (transform-objects transform-parameter-object (:parameters components-spec) "#/components/parameters" :OpenAPI-Parameter)
        (transform-objects transform-parameter-object (:headers components-spec) "#/components/headers" :OpenAPI-Header)
        (transform-objects transform-link-object (:links components-spec) "#/components/links" :OpenAPI-Link)
        (transform-objects transform-example-object (:examples components-spec) "#/components/examples" :OpenAPI-Example)
        (transform-objects transform-request-body-object (:requestBodies components-spec) "#/components/requestBodies" :OpenAPI-Request-body)
        )))


(def server-variable-fields [:enum :default])

(defn transform-server-variable [var-name var-spec]
  (let [var-spec (assoc var-spec :variableName (name var-name))]
    (common/render-resource-strings "templates/server-variable-object.tpl" var-spec server-variable-fields)))

(defn transform-server-variables [variables-spec]
  "appending markdown string descriptions"
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