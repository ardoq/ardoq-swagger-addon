(ns ardoq.swagger-test
  (:require [ardoq.swagger.client :as c]
            [ardoq.swagger.api :as api]
            [clojure.test :refer :all]
            [cheshire.core :refer [parse-string generate-string]]
            [clj-http.client :as http]
            [clojure.java.io :as io]))

(def ^:dynamic client nil)

(defn connect-ardoq [f]
  (binding [client (c/client {:url "http://localhost:8080";(System/getenv "BASE_URL")
                              :org "ardoq";(System/getenv "ORGANIZATION")
                              :token "451c4a98508e47f5997c6fd61a33ecff";(System/getenv "API_TOKEN")
                              })]
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
                 (c/map->Workspace)
                 (c/find-by-id client))]
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
  (c/delete (c/map->Component {:_id (first (:components swag))}) client)
  (let [json-spec (assoc-in json-spec [:definitions :new-model] {:type "object"})
        json-spec (assoc-in json-spec [:definitions :sec-model] {:type "object"})
        spec (generate-string json-spec)
        swag (-> {:_id (api/get-spec client nil wsname nil spec nil)}
                 (c/map->Workspace)
                 (c/find-by-id client))]
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
  (doall (take 5 (for [[name swag] (:body (http/get "https://apis-guru.github.io/api-models/api/v1/list.json" {:as :json}))] 
                   (let  [spec (->> swag 
                                    (:versions) 
                                    ((keyword (:preferred swag)))
                                    (:swaggerUrl)
                                    (http/get)
                                    (:body))
                          parsed-spec (parse-string spec true)
                          old-spec (import-spec spec nil parsed-spec name)]
                     (update-spec spec nil old-spec parsed-spec name)
                     (c/delete old-spec client))))))
