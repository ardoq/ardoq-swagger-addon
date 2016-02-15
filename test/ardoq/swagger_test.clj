(ns ardoq.swagger-test
  (:require [ardoq.swagger.client :as c]
            [ardoq.swagger.api :as api]
            [clojure.test :refer :all]
            [cheshire.core :refer [parse-string]]
            [clojure.java.io :as io]))

(def ^:dynamic client nil)

(defn connect-ardoq [f]
  (binding [client (c/client {:url (System/getenv "BASE_URL")
                              :org (System/getenv "ORGANIZATION")
                              :token (System/getenv "API_TOKEN")})]
    (f)))

(use-fixtures :each connect-ardoq)


(defn import-spec [spec wsname]  
  (let [swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (c/map->Workspace)
                 (c/find-by-id client))                 
        json-spec (parse-string spec true)]
    (is (= "BikeWise API v2" (:title (:info json-spec))))
    (c/delete swag client)
    ))


(deftest ^:integration import-swaggers
  ;;Loop all files in swagger folder and slurp
  ;;Import spec
  ;;Confirm wsname, model, components
  ;;Delete/alter first component for example then reupload the file
  ;;Delete workspace afterward
  (import-spec (slurp (io/resource "swagger/bikewise.org-v2-swagger.json")) nil)
  ;; (doall (pmap (fn [f] 
  ;;                (import-spec (slurp f) nil))
  ;;              (rest (file-seq (java.io.File. "resources/swagger")))))
                                        ;This probably needs fixing
  )

