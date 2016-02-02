(ns ardoq.swagger.swagger-v2
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.update-swagger2 :as update]
            [ardoq.swagger.swagger2-refs :as refs]
            [cheshire.core :refer [generate-string parse-string]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]))

(defn create-workspace [client model wsname {:keys [info] :as data}]
  ;; Creates a new workspace in the client. 
  (let [{:keys [_id]} model
        wsname (if (s/blank? wsname)
                 (:title info)
                 wsname)]
    (-> (api/->Workspace wsname (tpl/render-resource "infoTemplate.tpl" (assoc info :workspaceName wsname)) _id)
        (assoc :views ["swimlane" "sequence" "integrations" "componenttree" "relationships" "tableview" "tagscape" "reader" "processflow"])
        (api/create client))))

(defn parse-info [spec result]
  ;;Copies the info data from spec into result
  (assoc result 
         :info (:info spec)
         :paths (:paths spec)
         :definitions (:definitions spec)
         :produces (:produces spec)
         :consumes (:consumes spec)
         :parameters (:parameters spec)
         :security (:security spec)
         :securityDefinitions (:securityDefinitions spec)
         :tags (:tags spec)))

(defn create-tags [client {:keys [tags]} wid]
  (doall (reduce
          (fn [acc {:keys [name description]}]
            (assoc acc name (api/->Tag name description wid [] [])))
          {}
          tags)))

(defn create-models [model wid _id path {:keys [definitions]}]
  ;;Creates links between components
  (reduce
   (fn [acc [type schema]]
     (assoc acc (keyword type)
            (assoc
             (api/->Component type (common/model-template schema) (str wid) _id (api/type-id-by-name model "Model")  nil)
             :schema schema)))
   {}
   definitions))

(defn create-ops [client model models wid parent _id methods tags]
  (keep
   (fn [[method {parameters :parameters response :responses security :security tag :tags :as data}]]
     (if (not (= method (keyword "parameters")))
       (let [type (doall (map (fn [[_ v]]
                                (get-in v [:schema]))
                              response))
             op (-> (api/map->Component {:name (str (:name parent) "/" (name method)) 
                                         :description (common/generate-operation-description data models) 
                                         :rootWorkspace (str wid) 
                                         :model _id 
                                         :parent (:_id parent) 
                                         :method method
                                         :typeId (api/type-id-by-name model "Operation")}) 
                 (api/create client) 
                 (assoc :return-model type
                        :input-models parameters
                        :security security))]
         (common/find-or-create-tag client tag wid op tags)
         op)))
   methods))

(defn create-methods [client model models wid _id path spec {:keys [component]} methods tags] 
  ;; Used to create all methods for the resources and links them with the parent
  (common/update-comp client component spec)
  (create-ops client model models wid component _id methods tags))

(defn create-defs [client model {:keys [paths] :as spec} workspace]
  ;; Creates the models
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-models model wid _id paths spec)
        (common/save-models client nil))))

(defn create-param-model [wid _id parameters description model]
  (reduce
   (fn [acc [param schema]]
     (assoc acc (keyword param)
            (assoc
                (api/->Component param (common/generate-param-description schema) (str wid) _id (api/type-id-by-name model "Parameters") nil)
              :schema schema)))
   {}
   parameters))

(defn create-security [model wid _id sec-defs description]
  (reduce
   (fn [acc [sec schema]]
     (assoc acc (keyword sec)
            (assoc
             (api/->Component sec (common/generate-security-description schema) (str wid) _id (api/type-id-by-name model "securityDefinitions") nil)
             :schema schema)))
   {}
   sec-defs))

(defn create-security-defs [client model {:keys [securityDefinitions] :as spec} workspace]
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-security model wid _id securityDefinitions description)
        (common/save-models client nil))))

(defn create-params [client model {:keys [parameters] :as spec} workspace]
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-param-model wid _id parameters description model)
        (common/save-models client nil))))

(defn create-resource [client model {:keys [paths definitions] :as spec} defs params secur tags workspace]
  ;;Create a resource. Does so by setting first path resource then adding the operations to it. Requires a full swagger file as input and the workspace it is being created in
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (doseq [[path {:keys [parameters] :as methods}] paths]
      (let [parent (doall {:resource path
                           :parameters parameters
                           :component (-> (api/->Component path description (str wid) _id (api/type-id-by-name model "Resource") nil)
                               (api/create client))})
            operations (create-methods client model defs wid _id path spec parent methods tags)]
        (refs/create-resource-refs client parent params)
        (refs/create-refs client operations defs secur)))
    (refs/interdependent-model-refs client defs)
    (common/find-or-create-fields client model)))

(defn update-tags [client tags]
  (doseq
      [[_ tag] tags]
    (api/create tag client)))

(defn get-info [client wsname spec]
  (when-not (some-> (common/find-existing-resource client (if (s/blank? wsname) (:title (:info spec)) wsname) #(api/map->Workspace {}))
                  (api/find-aggregated client)
                  (update/update-workspace client spec))
    (let [model (common/find-or-create-model client "Swagger 2.0")
          workspace (create-workspace client model wsname spec)
          defs (create-defs client model spec workspace)
          params (create-params client model spec workspace)
          secur (create-security-defs client model spec workspace)         
          ;;To here
          tags-cache (atom (create-tags client spec (:_id workspace)))]
      (create-resource client model spec defs params secur tags-cache workspace)
      (update-tags client @tags-cache)
      (println "Done importing swagger doc.")
      (str (:_id workspace)))))


(defn import-swagger2 [client spec wsname]
  (println client)
  ;;Extracts data from a given Swagger file into an emtpy object
  (->> {}
       (parse-info spec)
       (get-info client wsname)))
