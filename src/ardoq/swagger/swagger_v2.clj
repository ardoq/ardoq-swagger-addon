(ns ardoq.swagger.swagger-v2
  (:import [java.net URI URL])
  (:require
   [ardoq.swagger.client :as api]
   [clojurewerkz.urly.core :as urly]
   [org.httpkit.client :as http]
   [clostache.parser :as tpl]
   [clojure.java.io :as io]
   [cheshire.core :refer [generate-string parse-string]]
   [clojure.string :as s]
   [medley.core :refer [map-vals]]))

(def client (api/client {:url "http://127.0.0.1:8080"
                       :token "9b2a9517e5c540a791f9db2468866a4f"
                       :org "ardoq"}))

(defn find-or-create-model [client]
  (if-let [model (first (filter #(= "Swagger" (:name %)) (api/find-all (api/map->Model {}) client)))]
    model
    (-> (api/map->Model (parse-string (slurp (io/resource "model.json")) true))
        (api/create client))))
  
(defn create-workspace [name client description]
  (let [{:keys [_id]} (find-or-create-model client)]
    (-> (api/->Workspace name description _id)
        (api/create client))
)
)

(defn parse-info [spec result]
  (assoc result :info (:info spec)))

(defn parse-paths [spec result]
  (assoc result :paths (:paths spec))
  )

(defn json-to-markdown [string]
  (doto string
    (println)
    (.replaceAll ":title(.*?)" "###$1\n")
    (println)))

(defn get-info [{:keys [:info]}]
  (let [{:keys [:version :title]} info]
    (-> ""
        (str "###")
        (str (get info :title)))
    ;(json-to-markdown (str info))
    )
        
    
;       (create-workspace "tester" client title)
    ;(println info)
    )


(defn get-data [spec]
  (->> {}
       (parse-info spec)
       (parse-paths spec)
;       (parse-definitions spec)
       (get-info)))




;;1 FindOrCreateModel -> Model
;;2 CreateWorkspace -> Workspace
;;3 CreateTopLevelComponnets -> List<Comonents>
;;
