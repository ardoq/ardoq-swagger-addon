;;;;Based on https://github.com/bripkens/json-schema-validation-example but modified to newer version.

;;The MIT License
;; Copyright (c) 2013 Ben Ripkens http://bripkens.de

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


(ns ardoq.swagger.validate
  (:require 
   [clojure.java.io :as io]
   [cheshire.core :refer [generate-string parse-string]])
  (:import 
   [com.fasterxml.jackson.databind ObjectMapper]
   [com.github.fge.jsonschema.core.report ProcessingMessage]
   [com.github.fge.jackson JacksonUtils]
   [com.github.fge.jsonschema.main JsonSchemaFactory]
   [com.github.fge.jsonschema.core.load.configuration LoadingConfiguration]
   [com.github.fge.jsonschema.core.load.uri URITranslatorConfiguration]))

(def json-schema-factory
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

(def object-reader
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
     :message (JacksonUtils/prettyPrint (.asJson report))}))

(defn validate-swagger [schema body]
  (validate schema body))
