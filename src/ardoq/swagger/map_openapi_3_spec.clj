(ns ardoq.swagger.map-openapi-3-spec
  (:require [ardoq.swagger.common :as common]
            [ardoq.swagger.map-common :refer :all]
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
            :open-api-specification "OpenAPI Specification"
            :spec-structure "OpenAPI Structure"
            :spec-server "OpenAPI Server"
            :spec-operation "OpenAPI Operation"
            :spec-path "OpenAPI Path"
            :spec-security-scheme "OpenAPI Security Scheme"
            :spec-callback "OpenAPI Callback"
            :spec-header "OpenAPI Header"
            :spec-response "OpenAPI Response"
            :spec-media-type "OpenAPI Media Type"
            :spec-request-body "OpenAPI Request Body"
            :spec-schema "OpenAPI Schema"
            :spec-parameter "OpenAPI Parameter"
            :spec-link "OpenAPI Link"
            :spec-example "OpenAPI Example"
            :spec-component "OpenAPI Component"
            :spec-security-requirement "OpenAPI Security Requirement"
            :spec-external-documentation "OpenAPI External Documentation"
            :spec-oauth-flow "OpenAPI OAuth Flow"
            :spec-xml "OpenAPI XML"
            :spec-discriminator "OpenAPI Discriminator"
            :orphan "Orphan"})


(defn render-security-requirements-object [security-requirements-object-spec]
  (common/render-resource-strings "templates/security-requirements.tpl" security-requirements-object-spec (keys security-requirements-object-spec)))


(defn render-discriminator-mapping [discriminator-mapping-spec]
  (common/render-resource-strings "templates/discriminator-mapping.tpl" discriminator-mapping-spec (keys discriminator-mapping-spec)))

(defn render-XML-object [xml-object-spec]
  (common/render-resource-strings "templates/xml-object.tpl" xml-object-spec (keys xml-object-spec)))

(declare transform-schema-list transform-schemas transform-parameter-object transform-server-object transform-callback-object)


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
                  (update-in [:swagger-object key] assoc :type :spec-structure)
                  (transform-schema-list schema-object-spec key))
              (let [schema-object-spec (assoc schema-object-spec :securityMd (render-security-requirements-object (:security schema-object-spec)))
                    schema-object-spec (assoc schema-object-spec :XMLMD (render-XML-object (:xml schema-object-spec)))
                    schema-object-spec (assoc schema-object-spec :hasDiscriminator (some? (:discriminator schema-object-spec)))
                    schema-object-spec (assoc schema-object-spec :discriminatorMappingMd (render-discriminator-mapping (get-in schema-object-spec [:discriminator :mapping])))
                    schema-object-spec (assoc schema-object-spec :hasExternalDocs (some? (:externalDocs schema-object-spec)))]
                (-> %
                  (update-in [:swagger-object key] assoc :type :spec-schema)
                  (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/schema-object.tpl" schema-object-spec schema-table-fields))
                  (transform-objects reference-security-schemes (:security schema-object-spec) key nil)
                  (transform-objects transform-schema-object (select-keys schema-object-spec [:items :allOf :oneOf :anyOf :not :additionalProperties]) key :spec-schema)
                  (transform-objects transform-schemas (select-keys schema-object-spec [:properties]) key :spec-structure)))))))))

(defn transform-schemas [schema-collection-key parent-key data schema-collection spec-type]
  (let [key (str parent-key "/" (name schema-collection-key))]
    (-> data
        (update-in [:swagger-object key] assoc :name (name schema-collection-key))
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :type spec-type)
        (transform-objects transform-schema-object schema-collection key :spec-schema))))


(defn transform-schema-list [data schema-list parent-key]
  (reduce
    (fn [data schema-object-spec]
      (let [key (or (:name schema-object-spec) (s/join ", " (:required schema-object-spec)) (:type schema-object-spec) "schema") ]
        (transform-schema-object key parent-key data schema-object-spec :spec-schema)))
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
          (transform-objects transform-oauth-flow-object (:flows security-scheme-object-spec) key :spec-oauth-flow)))))



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
        (transform-objects transform-parameter-object (:headers encoding-object-spec) key :spec-header))))

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
              (transform-server-object :server key % server-object-spec :spec-server)
              %))))))

(defn transform-media-type-object [media-type-key parent-key data media-type-object-spec spec-type]
  (let [key (str parent-key "/" (name media-type-key))]
    (-> data
        (update-in [:swagger-object key] assoc :name (name media-type-key))
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/media-type-object.tpl" media-type-object-spec []))
        (#(if-let [schema-object-spec (:schema media-type-object-spec)]
            (transform-schema-object :schema key % schema-object-spec :spec-schema)
            %))
        (transform-objects transform-encoding-object (:encoding media-type-object-spec) key :OpenAPI-Encoding)
        (transform-objects transform-example-object (:examples media-type-object-spec) key :spec-example))))


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
          (transform-objects transform-media-type-object (:content request-body-object-spec) key :spec-media-type)))))


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
          (transform-objects transform-media-type-object (:content parameter-object-spec) key :spec-media-type)
          (transform-objects transform-example-object (:examples parameter-object-spec) key :spec-example)
          (#(if-let [schema-object-spec (:schema parameter-object-spec)]
              (transform-schema-object :schema key % schema-object-spec :spec-schema)
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
          (transform-objects transform-parameter-object (:headers response-object-spec) key :spec-header)
          (transform-objects transform-media-type-object (:content response-object-spec) key :spec-media-type)
          (transform-objects transform-link-object (:links response-object-spec) key :spec-link)))))


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
        (transform-objects transform-parameter-object (:parameters operation-object-spec) key :spec-parameter)
        (#(if-let [request-body-spec (:requestBody operation-object-spec)]
            (transform-request-body-object :requestBody key % request-body-spec :spec-request-body)
            %))
        (transform-objects reference-security-schemes (:security operation-object-spec) key nil)
        (transform-objects transform-server-object (:servers operation-object-spec) key :spec-server)
        (transform-objects transform-callback-object (:callbacks operation-object-spec) key :spec-callback)
        (transform-objects transform-response-object (:responses operation-object-spec) key :spec-response))))


(defn transform-path-object [path-key parent-key data path-object-spec spec-type]
  (let [key (str parent-key "/" (name path-key))]
    (-> data
        (update-in [:swagger-object key] assoc :name (subs (str path-key) 1))
        (update-in [:swagger-object key] assoc :type :spec-path)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/path-object.tpl" path-object-spec))
        (transform-objects transform-operation-object (select-keys path-object-spec [:get :put :post :delete :options :head :patch :trace]) key :spec-operation)
        (transform-objects transform-parameter-object (:parameters path-object-spec) key :spec-parameter)
        (transform-objects transform-server-object (:servers path-object-spec) key :spec-server)
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
          (transform-objects transform-path-object callback-object-spec key :spec-path)))))


(defn transform-components-object [data spec]
  (let [components-spec (:components spec)]
    (-> data
        (transform-objects transform-schema-object (:schemas components-spec) "#/components/schemas" :spec-schema)
        (transform-objects transform-response-object (:responses components-spec) "#/components/responses" :spec-response)
        (transform-objects transform-parameter-object (:parameters components-spec) "#/components/parameters" :spec-parameter)
        (transform-objects transform-parameter-object (:headers components-spec) "#/components/headers" :spec-header)
        (transform-objects transform-link-object (:links components-spec) "#/components/links" :spec-link)
        (transform-objects transform-example-object (:examples components-spec) "#/components/examples" :spec-example)
        (transform-objects transform-request-body-object (:requestBodies components-spec) "#/components/requestBodies" :spec-request-body)
        (transform-objects transform-security-scheme-object (:securitySchemes components-spec) "#/components/securitySchemes" :spec-security-scheme)
        (transform-objects transform-callback-object (:callbacks components-spec) "#/components/callbacks" :spec-callback)
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

(defn create-scaffolding [data spec]
  (-> data
      (assoc-in [:swagger-object "#/components"] {:name "Components" :type :spec-structure :parent "#/"})
      (assoc-in [:swagger-object "#/components/schemas"] {:name "Schemas" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/responses"] {:name "Responses" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/parameters"] {:name "Parameters" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/examples"] {:name "Examples" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/requestBodies"] {:name "Request Bodies" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/headers"] {:name "Headers" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/securitySchemes"] {:name "Security Schemes" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/links"] {:name "Links" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/components/callbacks"] {:name "Callbacks" :type :spec-structure :parent "#/components"})
      (assoc-in [:swagger-object "#/paths"] {:name "Paths" :type :spec-structure :parent "#/"})
      (assoc-in [:swagger-object "#/servers"] {:name "Servers" :type :spec-structure :parent "#/"})))


(def info-fields
  [:title
   :version
   :termsOfService])

(def contact-object-fields
  [:name :email :url])

(def license-object-fields
  [:name :url])

(defn workspace-description [spec]
  (common/render-resource-strings "templates/workspace.tpl" {:importTime (.format (java.text.SimpleDateFormat. "yyyy.MM.dd HH:mm") (new java.util.Date))} []))

(defn create-root-element [data spec]
  (let [info-spec (:info spec)
        root-spec (-> {:contactMd       (common/render-resource-strings "templates/contact-object.tpl" (:contact info-spec) contact-object-fields)
                     :licenseMd       (common/render-resource-strings "templates/license-object.tpl" (:license info-spec) license-object-fields)
                     :hasExternalDocs (some? (:externalDocs info-spec))
                     :externalDocs    (:externalDocs info-spec)}
                    (merge (select-keys info-spec [:title :summary :description ]))
                    (merge (select-keys info-spec info-fields)))]
    (-> data
        (update-in [:swagger-object "#/"] assoc :type :open-api-specification)
        (update-in [:swagger-object "#/"] assoc :name (get-in spec [:info :title] ))
        (update-in [:swagger-object "#/"] assoc :description (common/render-resource-strings "templates/open-api-3-specification.tpl" root-spec info-fields)))))

(defn transform-spec [spec]
  ;;   {:key->swagger-object {{:name "n" :type "typename"} {:name "n" :description "descr"}}
  ;;    :references [{:source-path {} :target-path "#/components/schema/someName"}]
  ;;    :spec-path->key {"#/components/schema/someName" {:name "n" :type "typename"}}}

  (let [data {:title (get-in spec [:info :title])
              :swagger-object (maps/ordered-map)
              :references []
              :spec-path-to-key-map {}}]
    (->
      data
      (create-root-element spec)
      (create-scaffolding spec)
      (transform-objects transform-path-object (:paths spec) "#/paths" :spec-path)
      (transform-objects transform-server-object (:servers spec) "#/servers" :spec-server)
      (transform-components-object spec))))

(def transformer-definition
  {:model-file model-file
   :model-types model-types
   :workspace-description-fn workspace-description
   :spec-reference-type-name "OpenAPI $ref"
   :transform-spec-fn transform-spec})