(ns ardoq.swagger.swagger-v3
  (:require [ardoq.swagger.client :as api-client]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.socket :refer [socket-send]]
            [cheshire.core :refer [generate-string parse-string]]
            [org.httpkit.server :as srv]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]))











(defn import-swagger3 [client spec wsname]
  (let [ardoq-data
        (or
          (common/find-workspace-and-model client wsname :openapi-3.x)
          (common/create-workspace-and-model client wsname spec :openapi-3.x))]



    (clojure.pprint/pprint ardoq-data)
      ))



