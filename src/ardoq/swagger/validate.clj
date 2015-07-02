(ns ardoq.swagger.validate
  (:require 
   [clojure.java.io :as io]
   [cheshire.core :refer [generate-string parse-string]])
  (:import 
   [com.fasterxml.jackson.databind ObjectMapper]
   [com.github.fge.jsonschema.main JsonSchemaFactory]
   [com.github.fge.jsonschema.core.load.configuration LoadingConfiguration]
   [com.github.fge.jsonschema.core.load.uri URITranslatorConfiguration]))

(def
  ^{:private true
    :doc "An immutable and therefore thread-safe JSON schema factory.
         You can call (.getJsonSchema json-schema-factory <json-schema-node>)
         to retrieve a JsonSchema instance which can validate JSON."}
  json-schema-factory
  (let [transformer (-> (URITranslatorConfiguration/newBuilder)
                        (.setNamespace "resource:/schema/")
                        .freeze)
        loading-config (-> (LoadingConfiguration/newBuilder)
                           (.setURITranslatorConfiguration transformer)
                           .freeze)
        factory (-> (JsonSchemaFactory/newBuilder)
                    (.setLoadingConfiguration loading-config)
                    .freeze)]
    factory))

(def
  ^{:private true
    :doc "Initialize the object mapper first and keep it private as not all
         of its methods are thread-safe. Optionally configure it here.
         Reader instances are cheap to create."}
  object-reader
  (let [object-mapper (ObjectMapper.)]
    (fn [] (.reader object-mapper))))


(defn- parse-to-node
  "Parse the given String as JSON. Returns a Jackson JsonNode."
  [data] (-> (object-reader) (.readTree data)))


(defn- get-schema
  "Get the schema file's contents in form of a string. Function only expects
  the schema name, i.e. 'collection' or 'image'."
  [schema-name]
  (slurp (io/resource schema-name)))


(defn validate
  "Validates the given 'data' against the JSON schema. Returns an object
  with an :success property that equals true when the schema could
  successfully be validated. It additionally contains a :message property
  with a human readable error description."
  [schema-name data]
  (let [parsed-schema (parse-to-node (get-schema schema-name))
        schema (-> json-schema-factory (.getJsonSchema parsed-schema))
        parsed-data (parse-to-node data)
        report (.validate schema parsed-data)]
    {:success (.isSuccess report)
     :message (str report)}))

(defn validate-swagger [schema body]
  (validate schema body))
