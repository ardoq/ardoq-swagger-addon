(ns ardoq.swagger-test
  (:require [ardoq.core :as client]
            [ardoq.implement.api :as core]
            [ardoq.swagger.api :as api]
            [clojure.test :refer :all]
            [cheshire.core :refer [parse-string generate-string]]
            [clojure.java.io :as io]))

(def ^:dynamic client nil)

(defn connect-ardoq [f]
  (binding [client (client/client {:url (System/getenv "BASE_URL")
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
            (reduce (fn [inner [_ x]]                               
                         (+ inner (count (:security x))))
                       counter
                       method))
          0
          paths))

(defn import-spec [spec wsname json-spec f]  
  (let [swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (core/map->Workspace)
                 (core/find-by-id client))]
    (testing (str "Workspace name for " f)
      (is (= (:name swag) (:title (:info json-spec)))))
    (testing (str "Testing components in updated workspace when importing " f)
      (is (= (count (:components swag))
             (count-components json-spec))))
    (testing (str "Testing references in workspace when importing " f)
      (is (= (+ (count-references json-spec) 
                (count-securities (:paths json-spec)))
             (count (:references swag)))))
    swag))

(defn update-spec [spec wsname swag json-spec f]
  (core/delete (core/map->Component {:_id (first (:components swag))}) client)
  (let [json-spec (assoc-in json-spec [:definitions :new-model] {:type "object"})
        json-spec (assoc-in json-spec [:definitions :sec-model] {:type "object"})
        spec (generate-string json-spec)
        swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (core/map->Workspace)
                 (core/find-by-id client))]
    (testing (str "Workspace name for " f)
      (is (= (:name swag) (:title (:info json-spec)))))
    (testing (str "Testing components in updated workspace when importing " f)
      (is (= (count (:components swag))
             (count-components json-spec))))
    (testing (str "Testing references in workspace when importing " f)
      (is (= (+ (count-references json-spec) 
                (count-securities (:paths json-spec)))
             (count (:references swag)))))))

(deftest import-swaggers
  (doall (for [f (file-seq (io/as-file (io/resource "swagger")))]
           (when-not (.isDirectory f)
             (do (println (str "Doing file " f))
                 (let [spec (slurp f)
                       json-spec (parse-string spec true)
                       swag (import-spec spec nil json-spec f)]
                   (update-spec spec nil swag json-spec f)
                   (core/delete swag client)))))))
