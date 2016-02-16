(ns ardoq.swagger-test
  (:require [ardoq.swagger.client :as c]
            [ardoq.swagger.api :as api]
            [clojure.test :refer :all]
            [cheshire.core :refer [parse-string generate-string]]
            [clojure.java.io :as io]))

(def ^:dynamic client nil)

(defn connect-ardoq [f]
  (binding [client (c/client {:url (System/getenv "BASE_URL")
                              :org (System/getenv "ORGANIZATION")
                              :token (System/getenv "API_TOKEN")})]
    (f)))

(use-fixtures :once connect-ardoq)


(defn count-components [spec]
  (reduce (fn [c [p v]]
            (+ c (- (count v) (if (:$ref v) 1 0) (if (:parameters v) 1 0))))
          (+ (count (:paths spec)) 
             (count (:definitions spec)) 
             (count (:securityDefinitions spec)) 
             (count (:parameters spec)))
          (:paths spec)))

(defn import-spec [spec wsname json-spec]  
  (let [swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (c/map->Workspace)
                 (c/find-by-id client))]
    (testing "Newly created workspace"
      (is (= (:name swag) (:title (:info json-spec))))
      (is (= (count (:components swag))
             (count-components json-spec))))
    swag))

(defn update-spec [spec wsname swag json-spec]
  (c/delete (c/map->Component {:_id (first (:components swag))}) client)
  (let [json-spec (assoc-in json-spec [:definitions :new-model] {:type "object"})
        json-spec (assoc-in json-spec [:definitions :sec-model] {:type "object"})
        spec (generate-string json-spec)
        swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (c/map->Workspace)
                 (c/find-by-id client))]
    (testing "Testing updated workspace"
      (is (= (:name swag) (:title (:info json-spec))))
      (is (= (count (:components swag))
             (count-components json-spec))))))

(deftest import-swaggers
  (doall (take 5 (for [f (.listFiles (java.io.File. "resources/swagger"))]
                   (when-not (.isDirectory f)
                     (let [spec (slurp f)
                           json-spec (parse-string spec true)
                           swag (import-spec spec nil json-spec)] 
                       (update-spec spec nil swag json-spec)
                       (c/delete swag client)))))))

