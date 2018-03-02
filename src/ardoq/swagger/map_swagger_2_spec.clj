(ns ardoq.swagger.map-swagger-2-spec
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

(def model-file "model-swagger-2.x.json")

(def model-types {
                  :swagger-specification "Swagger Specification"
                  :spec-structure "Swagger Structure"
                  :spec-server "Swagger Server"
                  :spec-operation "Swagger Operation"
                  :spec-path "Swagger Path"
                  :spec-security-scheme "Swagger Security Scheme"
                  :spec-callback "Swagger Callback"
                  :spec-header "Swagger Header"
                  :spec-response "Swagger Response"
                  :spec-media-type "Swagger Media Type"
                  :spec-request-body "Swagger Request Body"
                  :spec-schema "Swagger Schema"
                  :spec-parameter "Swagger Parameter"
                  :spec-link "Swagger Link"
                  :spec-example "Swagger Example"
                  :spec-items "Swagger Items"
                  :spec-security-requirement "Swagger Security Requirement"
                  :spec-external-documentation "Swagger External Documentation"
                  :spec-oauth-flow "Swagger OAuth Flow"
                  :spec-xml "Swagger XML"
                  :spec-discriminator "Swagger Discriminator"
                  :orphan "Orphan"})


(declare transform-schema-list transform-schemas)

(defn render-security-requirements-object [security-requirements-object-spec]
  (common/render-resource-strings "templates/security-requirements.tpl" security-requirements-object-spec (keys security-requirements-object-spec)))

(defn render-XML-object [xml-object-spec]
  (common/render-resource-strings "templates/xml-object.tpl" xml-object-spec (keys xml-object-spec)))

(defn reference-security-schemes [security-requirement-key parent-key data schema-object-spec _]
  (let [security-scheme-key (str "#/securityDefinitions" (name security-requirement-key))]
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
   :discriminator
   :readOnly
   :example])

(defn transform-schema-object [schema-key parent-key data schema-object-spec spec-type]
  (let [key (str parent-key "/" (name schema-key))]
    (if-let [ref (:$ref schema-object-spec)]
      (-> data
          (update-in [:references] conj {:source-path parent-key :target-path ref}))
      (-> data
          (update-in [:swagger-object key] assoc :name (name schema-key))
          (update-in [:swagger-object key] assoc :parent parent-key)
          (#(if (vector? schema-object-spec)
              (-> %
                  (update-in [:swagger-object key] assoc :type :spec-structure)
                  (transform-schema-list schema-object-spec key))
              (let [schema-object-spec (assoc schema-object-spec :securityMd (render-security-requirements-object (:security schema-object-spec)))
                    schema-object-spec (assoc schema-object-spec :XMLMD (render-XML-object (:xml schema-object-spec)))
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



;; used for both :spec-items and :spec-headers, since these contain a subset of the fields and nested children
(def parameter-table-fields
  [:in
   :required
   :type
   :format
   :deprecated
   :allowEmptyValue
   :collectionFormat
   :default
   :maximum
   :exclusiveMinimum
   :minimum
   :exclusiveMinimum
   :maxLength
   :minLength
   :pattern
   :maxItems
   :minItems
   :uniqueItems
   :enum
   :multipleOf])

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
          (#(if-let [items-object-spec (:items parameter-object-spec)]
              (transform-parameter-object :items key % items-object-spec :spec-items)
              %))
          (#(if-let [schema-object-spec (:schema parameter-object-spec)]
              (transform-schema-object :schema key % schema-object-spec :spec-schema)
              %))))))


(def security-scheme-fields
  [:type
   :in
   :scheme
   :flow
   :authorizationUrl
   :tokenUrl])

(defn transform-security-scheme-object [scheme-key parent-key data security-scheme-object-spec spec-type]
  (let [key (str parent-key "/" (name scheme-key))
        scopes-spec (:scopes security-scheme-object-spec)
        scopes-md (common/render-resource-strings "templates/oauth-scopes.tpl" scopes-spec (keys scopes-spec))
        security-scheme-object-spec (assoc security-scheme-object-spec :scopesMd scopes-md)]
    (-> data
        (update-in [:swagger-object key] assoc :name (name scheme-key))
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/security-scheme-object.tpl" security-scheme-object-spec security-scheme-fields)))))


(defn transform-example-object [encoding-key parent-key data example-object-spec spec-type]
  (if-let [ref (:$ref example-object-spec)]
    (-> data
        (update-in [:references] conj {:source-path parent-key :target-path ref}))
    (let [key (str parent-key "/" (name encoding-key))]
      (-> data
          (update-in [:swagger-object key] assoc :name (name encoding-key))
          (update-in [:swagger-object key] assoc :type spec-type)
          (update-in [:swagger-object key] assoc :parent parent-key)
          (update-in [:swagger-object key] assoc :description (str "```\n" example-object-spec "```\n"))))))


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
          (#(if-let [example-object-spec (:example response-object-spec)]
             (transform-example-object :example key % example-object-spec :spec-example)
             %))
          (#(if-let [schema-object-spec (:schema response-object-spec)]
              (transform-schema-object :schema key % schema-object-spec :spec-schema)
              %))))))


(def operation-fields
  [:tags
   :operationId
   :consumes
   :produces
   :schemes
   :deprecated])

(defn transform-operation-object [operation-key parent-key data operation-object-spec spec-type]
  (let [key (str parent-key "/" (name operation-key))
        security-requirements (into {} (:security operation-object-spec))
        operation-object-spec (assoc operation-object-spec :securityMd (render-security-requirements-object security-requirements))
        operation-object-spec (assoc operation-object-spec :hasExternalDocs (some? (:externalDocs operation-object-spec)))]
    (-> data
        (update-in [:swagger-object key] assoc :name (name operation-key))
        (update-in [:swagger-object key] assoc :type spec-type)
        (update-in [:swagger-object key] assoc :parent parent-key)
        (update-in [:swagger-object key] assoc :description (common/render-resource-strings "templates/operation-object.tpl" operation-object-spec operation-fields))
        (transform-objects transform-parameter-object (:parameters operation-object-spec) key :spec-parameter)
        (transform-objects reference-security-schemes (:security operation-object-spec) key :_)
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
        (#(if-let [ref (:$ref path-object-spec)]
            (update-in % [:references] conj {:source-path key :target-path ref})
            %)))))



(defn create-scaffolding [data spec]
  (-> data
      (assoc-in [:swagger-object "#/paths"] {:name "Paths" :type :spec-structure :parent "#/"})
      (assoc-in [:swagger-object "#/definitions"] {:name "Definitions" :type :spec-structure :parent "#/"})
      (assoc-in [:swagger-object "#/parameters"] {:name "Parameters" :type :spec-structure :parent "#/"})
      (assoc-in [:swagger-object "#/responses"] {:name "Responses" :type :spec-structure :parent "#/"})
      (assoc-in [:swagger-object "#/securityDefinitions"] {:name "SecurityDefinitions" :type :spec-structure :parent "#/"})
      (assoc-in [:swagger-object "#/security"] {:name "Security" :type :spec-structure :parent "#/"})))


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
                     :securityReqsMd  (render-security-requirements-object (into {} (:security spec)))
                     :hasExternalDocs (some? (:externalDocs info-spec))
                     :externalDocs    (:externalDocs info-spec)}
                    (merge (select-keys info-spec [:title :summary :description ]))
                    (merge (select-keys info-spec info-fields)))]
    (-> data
        (update-in [:swagger-object "#/"] assoc :type :swagger-specification)
        (update-in [:swagger-object "#/"] assoc :name (get-in spec [:info :title] ))
        (update-in [:swagger-object "#/"] assoc :description (common/render-resource-strings "templates/swagger-specification.tpl" root-spec info-fields)))))

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
      (transform-objects transform-schema-object (:definitions spec) "#/definitions" :spec-schema)
      (transform-objects transform-parameter-object (:parameters spec) "#/parameters" :spec-parameter)
      (transform-objects transform-response-object (:responses spec) "#/responses" :spec-response)
      (transform-objects transform-security-scheme-object (:securityDefinitions spec) "#/securityDefinitions" :spec-security-scheme))))

(def transformer-definition
  {:model-file model-file
   :model-types model-types
   :workspace-description-fn workspace-description
   :spec-reference-type-name "Swagger $ref"
   :transform-spec-fn transform-spec})