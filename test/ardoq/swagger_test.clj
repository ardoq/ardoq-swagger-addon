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

(defn count-operations [paths]
  (reduce (fn [c [p v]]
            (+ c (- (count v) (if (:$ref v) 1 0) (if (:parameters v) 1 0))))
          0
          paths))

(defn count-components [spec]
  (+ (count-operations (:paths spec))
     (count (:paths spec)) 
     (count (:definitions spec)) 
     (count (:securityDefinitions spec)) 
     (count (:parameters spec))))

(defn count-references [spec]
  (reduce (fn [result x] 
            (+ result 
               (cond 
                (= (keyword "$ref") x) 1
                (map? x) (count-references x)
                :else 0)))
          0
          (flatten (seq spec))))

(defn count-securities [paths]
  (reduce (fn [counter [_ method]]
           (+ counter (reduce (fn [inner [_ x]]                               
                                (+ inner (count (:security x))))
                         0
                         method)))
          0
          paths))

(defn import-spec [spec wsname json-spec]  
  (let [swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (c/map->Workspace)
                 (c/find-by-id client))]
    (testing "Workspace name"
      (is (= (:name swag) (:title (:info json-spec)))))
    (testing "Testing components in workspace"
      (is (= (count (:components swag))
             (count-components json-spec))))
     (testing "Testing references in workspace"
      (is (= (+ (count-references json-spec) 
                (count-securities (:paths json-spec)))
             (count (:references swag)))))
    swag))

(defn update-spec [spec wsname swag json-spec]
  (c/delete (c/map->Component {:_id (first (:components swag))}) client)
  (let [json-spec (assoc-in json-spec [:definitions :new-model] {:type "object"})
        json-spec (assoc-in json-spec [:definitions :sec-model] {:type "object"})
        spec (generate-string json-spec)
        swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (c/map->Workspace)
                 (c/find-by-id client))]
    (testing "Workspace name"
      (is (= (:name swag) (:title (:info json-spec)))))
    (testing "Testing components in updated workspace"
      (is (= (count (:components swag))
             (count-components json-spec))))
    (testing "Testing references in workspace"
      (is (= (+ (count-references json-spec) 
                (count-securities (:paths json-spec)))
             (count (:references swag)))))))

(deftest import-swaggers
  (doall (take 5 (for [f (file-seq (io/as-file (io/resource "swagger")))]
                   (when-not (.isDirectory f)
                     (let [spec (slurp f)
                           json-spec (parse-string spec true)
                           swag (import-spec spec nil json-spec)]
                       (update-spec spec nil swag json-spec)
                       (c/delete swag client)))))))

