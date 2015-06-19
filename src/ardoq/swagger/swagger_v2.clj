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

(defn contact-str [{:keys [:contact]}]
  ;;Converts the contact part of the info to a markdown table. Needs fixing
  (let [{:keys [name url email] :or {name "N/A" url "N/A" email "N/A" }} contact] 
    (->> ""
         (str "| \n")
         (str (str " | " email))
         (str (str " | " url))
         (str (str " | " name))
         (str "| Name | Url | E-mail | \n")
         (str "#####Contact info\n")))
)

(defn license-str [{:keys [:license]}]
  ;;Converts the license part of the info to a markdown table. Needs fixing
  (let [{:keys [name url] :or {url "N/A"}} license]
    (->> ""
         (str "| \n")
         (str (str " | " url))
         (str (str " | " name))
         (str "| Name | Url | \n")
         (str "#####License info\n")))
)

(defn json-to-markdown [info string]
  ;;Converts specific parts. If the part doesn't excist the regex fails and does nothing
  (-> string
    (.replaceAll "[ {]*:title \\\"(.*?)\\\"[,}]" "###$1\n")
    (.replaceAll "[ {]*:version \\\"(.*?)\\\"[,}]" "Version: $1\n\n")
    (.replaceAll "[ {]*:description \\\"(.*?)\\\"[,}]" "$1\n\n")
    (.replaceAll "[ {]*:termsOfService \\\"(.*?)\\\"[,}]" "$1\n\n")
    (.replaceAll "[ {]*:contact [{](.*?)[}][,}]" (contact-str info))
    (.replaceAll "[ {]*:license [{](.*?)[}][,}]" (license-str info))
    ))

(defn update-comp [component {:keys [produces consumes]}]
  (clojure.pprint/pprint component)
  (api/update 
   (cond-> (api/map->Component component)
     produces (assoc :produces produces)
     consumes (assoc :consumes consumes)) client))

(defn create-methods [parent model description wid _id path [methods]]
  (-> (api/->Component methods description (str wid) _id (api/type-id-by-name model "Operation") (str (:_id parent))) 
      (api/create client))
  )

(defn create-resource [{:keys [paths]} workspace]
  ;;Create a resource. Does so by setting first path resource then adding the operations to it. Requires a full swagger file as input and the workspace it is being created in
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (map 
     (fn [[path methods]]
       (let [parent (-> (api/->Component path description (str wid) _id (api/type-id-by-name model "Resource") nil)
                        (api/create client))]
         ;; (map 
         ;;  (fn [[m]]
         ;;    (-> (api/->Component m description (str wid) _id (api/type-id-by-name model "Operation") (str (:_id parent)))
         ;;        (api/create client))) 
         ;;  methods)
         (map (partial create-methods parent model description wid _id path) methods)
         ))
     paths)))

(defn get-info [spec]
  ;Converts the info from a Swagger 2 map to a string - This method needs to be redone
  (let [{:keys [:info]} spec
        workspace (->> ""
             (str info)
             (json-to-markdown info)
             (create-workspace "tester" client)
             )]
    (create-resource spec workspace)))


(defn get-data [spec]
  ;;Extracts data from a given Swagger file into an emtpy object
  (->> {}
       (parse-info spec)
       (parse-paths spec)
;       (parse-definitions spec)
       (get-info)

       ))




;;1 FindOrCreateModel -> Model
;;2 CreateWorkspace -> Workspace
;;3 CreateTopLevelComponnets -> List<Comonents>
;;
