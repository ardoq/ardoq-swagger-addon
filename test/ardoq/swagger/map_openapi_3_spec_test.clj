(ns ardoq.swagger.map-openapi-3-spec-test
  (:require
    [ardoq.swagger.map-openapi-3-spec :as map-openapi-3-spec]
    [ardoq.swagger.map-common :as map-common]
    [superstring.core :as str]
    [ardoq.swagger.util :as util]
    [clojure.test :refer :all]))


(def parent-key "#/parent")

(def initial-data {:should "be preserved"})


(defn read-spec [name]
  (util/parse-swagger (slurp (str "test/ardoq/swagger/" name))))


(deftest transform-schema-object-test
  (let [schema-object-spec (read-spec "schema-object-spec.yaml")
        data (map-common/transform-objects initial-data map-openapi-3-spec/transform-schema-object schema-object-spec "#/components/schemas" :OpenAPI-Schema)]

    (is (= "Pet" (get-in data [:swagger-object "#/components/schemas/Pet" :name])))
    (is (str/contains? (get-in data [:swagger-object "#/components/schemas/Pet/allOf/id" :description]) "required"))
    (is (= 1 (count (:references data))))))





