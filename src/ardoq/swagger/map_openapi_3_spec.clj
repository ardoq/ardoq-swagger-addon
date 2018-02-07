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
            :OpenAPI-OAuth-Flow "OpenAPI OAuth Flow"
            :OpenAPI-XML "OpenAPI XML"
            :OpenAPI-Discriminator "OpenAPI Discriminator"
            :Orphan "Orphan"})


(defn transform-objects [data transform-object-fn param-spec parent-key spec-type]
  (reduce
    (fn [data object-spec]
      (if (map-entry? object-spec)
        (apply transform-object-fn [(key object-spec) parent-key data (val object-spec) spec-type])
        (apply transform-object-fn [(or (:name object-spec) (name spec-type)) parent-key data object-spec spec-type])))
    data
    param-spec))

(defn render-security-requirements-object [security-requirements-object-spec]
  (common/render-resource-strings "templates/security-requirements.tpl" security-requirements-object-spec (keys security-requirements-object-spec)))


(defn render-discriminator-mapping [discriminator-mapping-spec]
  (common/render-resource-strings "templates/discriminator-mapping.tpl" discriminator-mapping-spec (keys discriminator-mapping-spec)))

(defn render-XML-object [xml-object-spec]
  (common/render-resource-strings "templates/xml-object.tpl" xml-object-spec (keys xml-object-spec)))

(declare transform-schema-list transform-parameter-object transform-server-object transform-callback-object)


(defn reference-security-schemes [security-requirement-key parent-key data schema-object-spec spec-type]
  (let [security-scheme-key (str "#/components/securitySchemes/" (name security-requirement-key))]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path security-scheme-key}))))

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
   :readOnly
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
              (let [schema-object-spec (assoc schema-object-spec :securityMd (render-security-requirements-object (:security schema-object-spec)))
                    schema-object-spec (assoc schema-object-spec :XMLMD (render-XML-object (:xml schema-object-spec)))
                    schema-object-spec (assoc schema-object-spec :hasDiscriminator (some? (:discriminator schema-object-spec)))
                    schema-object-spec (assoc schema-object-spec :discriminatorMappingMd (render-discriminator-mapping (get-in schema-object-spec [:discriminator :mapping])))
                    schema-object-spec (assoc schema-object-spec :hasExternalDocs (some? (:externalDocs schema-object-spec)))]
                (-> %
                  (update-in [:swagger-object key] assoc :type :OpenAPI-Schema)
                  (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/schema-object.tpl" schema-object-spec schema-table-fields))
                  (transform-objects reference-security-schemes (:security schema-object-spec) key nil)
                  (transform-objects transform-schema-object (select-keys schema-object-spec [:items :allOf :oneOf :anyOf :not :properties :additionalProperties]) key :OpenAPI-Schema)))))))))


(defn transform-schema-list [data schema-list parent-key]
  (reduce
    (fn [data schema-object-spec]
      (let [key (or (:name schema-object-spec) (s/join ", " (:required schema-object-spec)) (:type schema-object-spec) "schema") ]
        (transform-schema-object key parent-key data schema-object-spec :OpenAPI-Schema)))
    data
    schema-list))

(def oauth-flow-fields
  [:authorizationUrl
   :tokenUrl
   :refreshUrl])

(defn transform-oauth-flow-object [flow-key parent-key data oauth-flow-object-spec spec-type]
  (let [key (str parent-key "/" (name flow-key))
        scopes-spec (:scopes oauth-flow-object-spec)
        scopes-md (common/render-resource-strings "templates/oauth-scopes.tpl" scopes-spec (keys scopes-spec))
        oauth-flow-object-spec (assoc oauth-flow-object-spec :scopesMd scopes-md)]
    (-> data
        (update-in [:swagger-object key] assoc :name (name flow-key))
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/oauth-flow-object.tpl" oauth-flow-object-spec oauth-flow-fields)))))


(def security-scheme-fields
  [:type
   :in
   :scheme
   :bearerFormat
   :openIdConnectUrl])

(defn transform-security-scheme-object [scheme-key parent-key data security-scheme-object-spec spec-type]
  (if-let [ref (:$ref security-scheme-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" (name scheme-key))]
      (-> data
          (update-in [:swagger-object key] assoc :name (name scheme-key))
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/security-scheme-object.tpl" security-scheme-object-spec security-scheme-fields))
          (transform-objects transform-oauth-flow-object (:flows security-scheme-object-spec) key :OpenAPI-OAuth-Flow)))))



(defn transform-example-object [encoding-key parent-key data example-object-spec spec-type]
  (if-let [ref (:$ref example-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" (name encoding-key))
          example-object-spec (assoc example-object-spec :hasValue (some? (:value example-object-spec)))
          example-object-spec (assoc example-object-spec :hasExternalValue (some? (:externalValue example-object-spec)))]
      (-> data
          (update-in [:swagger-object key] assoc :name (name encoding-key))
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/example-object.tpl" example-object-spec []))))))


(def encoding-table-fields
  [:contentType
   :style
   :explode
   :allowReserved])

(defn transform-encoding-object [encoding-key parent-key data encoding-object-spec spec-type]
  (let [key (str parent-key "/" (name encoding-key))]
    (-> data
        (update-in [:swagger-object key] assoc :name (name encoding-key))
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/encoding-object.tpl" encoding-object-spec encoding-table-fields))
        (transform-objects transform-parameter-object (:headers encoding-object-spec) key :OpenAPI-Header))))

(def link-table-fields
  [:operationRef
   :operationId
   :requestBody])

(defn transform-link-object [link-key parent-key data link-object-spec spec-type]
  (if-let [ref (:$ref link-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" (name link-key))
          link-parameters (:parameters link-object-spec)
          link-parameters-md (common/render-resource-strings "templates/link-parameters.tpl" link-parameters (keys link-parameters))
          link-object-spec (assoc link-object-spec :linkParameters link-parameters-md)]
      (-> data
          (update-in [:swagger-object key] assoc :name (name link-key))
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/link-object.tpl" link-object-spec link-table-fields))
          (#(if-let [server-object-spec (:server link-object-spec)]
              (transform-server-object :server key % server-object-spec :OpenAPI-Server)
              %))))))

(defn transform-media-type-object [media-type-key parent-key data media-type-object-spec spec-type]
  (let [key (str parent-key "/" (name media-type-key))]
    (-> data
        (update-in [:swagger-object key] assoc :name (name media-type-key))
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

(defn transform-request-body-object [request-body-key parent-key data request-body-object-spec spec-type]
  (if-let [ref (:$ref request-body-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" (name request-body-key))]
      (-> data
          (update-in [:swagger-object key] assoc :name (name request-body-key))
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


(defn transform-parameter-object [parameter-key parent-key data parameter-object-spec spec-type]
  (if-let [ref (:$ref parameter-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" (name parameter-key))]
      (-> data
          (update-in [:swagger-object key] assoc :name (name parameter-key))
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

(defn transform-operation-object [operation-key parent-key data operation-object-spec spec-type]
  (let [key (str parent-key "/" (name operation-key))
        security-requirements (:security operation-object-spec)
        operation-object-spec (assoc operation-object-spec :securityMd (render-security-requirements-object security-requirements))
        operation-object-spec (assoc operation-object-spec :hasExternalDocs (some? (:externalDocs operation-object-spec)))]
    (-> data
        (update-in [:swagger-object key] assoc :name (name operation-key))
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/operation-object.tpl" operation-object-spec operation-fields))
        (transform-objects transform-parameter-object (:parameters operation-object-spec) key :OpenAPI-Parameter)
        (#(if-let [request-body-spec (:requestBody operation-object-spec)]
            (transform-request-body-object :requestBody key % request-body-spec :OpenAPI-Request-body)
            %))
        (transform-objects reference-security-schemes (:security operation-object-spec) key nil)
        (transform-objects transform-server-object (:servers operation-object-spec) key :OpenAPI-Server)
        (transform-objects transform-callback-object (:callbacks operation-object-spec) key :OpenAPI-Callback)
        (transform-objects transform-response-object (:responses operation-object-spec) key :OpenAPI-Response))))


(defn transform-path-object [path-key parent-key data path-object-spec spec-type]
  (let [key (str parent-key "/" (name path-key))]
    (-> data
        (update-in [:swagger-object key] assoc :name (subs (str path-key) 1))
        (update-in [:swagger-object key] assoc :type :OpenAPI-Path)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/path-object.tpl" path-object-spec))
        (transform-objects transform-operation-object (select-keys path-object-spec [:get :put :post :delete :options :head :patch :trace]) key :OpenAPI-Operation)
        (transform-objects transform-parameter-object (:parameters path-object-spec) key :OpenAPI-Parameter)
        (transform-objects transform-server-object (:servers path-object-spec) key :OpenAPI-Server)
        (#(if-let [ref (:$ref path-object-spec)]
            (update-in % [:references] conj {:source-path key :target-path ref})
            %)))))


(defn transform-callback-object [callback-key parent-key data callback-object-spec spec-type]
  (if-let [ref (:$ref callback-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" (name callback-key))]
      (-> data
          (update-in [:swagger-object key] assoc :name (name callback-key))
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (transform-objects transform-path-object callback-object-spec key :OpenAPI-Path)))))


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
        (transform-objects transform-security-scheme-object (:securitySchemes components-spec) "#/components/securitySchemes" :OpenAPI-Security-Scheme)
        (transform-objects transform-callback-object (:callbacks components-spec) "#/components/callbacks" :OpenAPI-Callback)
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

(defn transform-server-object [_ parent-key data server-object-spec spec-type]
  (let [key (str parent-key "/" (:url server-object-spec))
        render-data {:description (:description server-object-spec)
                     :variables (transform-server-variables (:variables server-object-spec))}]

    (-> data
        (update-in [:swagger-object key] assoc :name (:url server-object-spec))
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/server-object.tpl" render-data)))))

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
        info-spec (assoc info-spec :license license-object-md)
        info-spec (assoc info-spec :hasExternalDocs (some? (:externalDocs info-spec)))]
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
      (transform-objects transform-path-object (:paths spec) "#/paths" :OpenAPI-Path)
      (transform-objects transform-server-object (:servers spec) "#/servers" :OpenAPI-Server)
      (transform-components-object spec))))

(def transformer-definition
  {:model-file model-file
   :model-types model-types
   :workspace-description-fn workspace-description
   :transform-spec-fn transform-spec})