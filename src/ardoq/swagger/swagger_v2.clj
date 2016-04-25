(ns ardoq.swagger.swagger-v2
  (:require [ardoq.impl.api :as api]
            [ardoq.core :as c]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.update-swagger2 :as update]
            [ardoq.swagger.swagger2-refs :as refs]
            [ardoq.swagger.socket :refer [socket-send]]
            [cheshire.core :refer [generate-string parse-string]]
            [org.httpkit.server :as srv]
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
    (api/create client 
                (-> (api/->Workspace wsname (tpl/render-resource "infoTemplate.tpl" (assoc info :workspaceName wsname)) _id)
                    (assoc :views ["swimlane" "sequence" "integrations" "componenttree" "relationships" "tableview" "tagscape" "reader" "processflow"])))))

(defn create-tags [client {:keys [tags]} wid]
  (doall (reduce
          (fn [acc {:keys [name description]}]
            (assoc acc name (api/->Tag name description wid [] [])))
          {}
          tags)))

(defn create-models [client model wid _id path {:keys [definitions]}]
  ;;Creates links between components
  (reduce
   (fn [acc [type schema]]
     (assoc acc (keyword type)
            (assoc
                (api/map->Component {:name type :description (common/model-template schema) :rootWorkspace (str wid) :model _id :type (c/component-type-id-by-name client (:_id model) "Model") :parent nil})
              :schema schema)))
   {}
   definitions))

(defn create-ops [client model models wid parent _id methods tags]
  (doall (keep
           (fn [[method {parameters :parameters response :responses security :security tag :tags :as data}]]
             (if (not (= method (keyword "parameters")))
               (let [type (doall (map (fn [[_ v]]
                                        (get-in v [:schema]))
                                      response))
                     op (-> (api/create client 
                                        (api/map->Component 
                                         {:name (str (:name parent) "/" (name method)) 
                                          :description (common/generate-operation-description data models) 
                                          :rootWorkspace (str wid) 
                                          :model _id 
                                          :parent (:_id parent) 
                                          :method method
                                          :typeId (c/component-type-id-by-name client (:_id model) "Operation")})) 
                            (assoc :return-model type
                                   :input-models parameters
                                   :security security))]
                 (common/find-or-create-tag client tag wid op tags)
                 op)))
           methods)))

(defn create-methods [client model models wid _id path spec {:keys [component]} methods tags] 
  ;; Used to create all methods for the resources and links them with the parent
  (common/update-comp client component spec)
  (create-ops client model models wid component _id methods tags))

(defn create-defs [client model {:keys [paths] :as spec} workspace]
  ;; Creates the models
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-models client model wid _id paths spec)
        (common/save-models client nil))))

(defn create-param-model [client wid _id parameters description model]
  (reduce
   (fn [acc [param schema]]
     (assoc acc (keyword param)
            (assoc
                (api/map->Component 
                 {:name (name param) :description (common/generate-param-description schema) :rootWorkspace (str wid) :model _id :type (c/component-type-id-by-name client (:_id model) "Parameters") :parent nil})
              :schema schema)))
   {}
   parameters))

(defn create-security [client model wid _id sec-defs description]
  (reduce
   (fn [acc [sec schema]]
     (assoc acc (keyword sec)
            (assoc
                (api/map->Component {:name sec :description (common/generate-security-description schema) :rootWorkspace (str wid) :model _id :type (c/component-type-id-by-name client (:_id model) "securityDefinitions") :parent nil})
             :schema schema)))
   {}
   sec-defs))

(defn create-security-defs [client model {:keys [securityDefinitions] :as spec} workspace]
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-security client model wid _id securityDefinitions description)
        (common/save-models client nil))))

(defn create-params [client model {:keys [parameters] :as spec} workspace]
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-param-model client wid _id parameters description model)
        (common/save-models client nil))))

(defn create-resource [client model {:keys [paths definitions] :as spec} defs params secur tags workspace]
  ;;Create a resource. Does so by setting first path resource then adding the operations to it. Requires a full swagger file as input and the workspace it is being created in
  (let [{:keys [_id description]} model
        wid (:_id workspace)]
    (doseq [[path {:keys [parameters]:as methods}] paths]
      (let [parent (doall {:resource path
                           :parameters parameters
                           :component (->> (api/map->Component {:name path :description description :rootWorkspace (str wid) :model _id :type (c/component-type-id-by-name client (:_id model) "Resource") :parent nil})
                               (api/create client))})
            operations (create-methods client model defs wid _id path spec parent methods tags)]
        (socket-send (str "Created " (count operations) " operations for resource " path))
        (refs/create-resource-refs client parent params)
        (refs/create-refs client operations defs secur)))
    (refs/interdependent-model-refs client defs)
    (common/find-or-create-fields client model)))

(defn update-tags [client tags]
  (doseq
      [[_ tag] tags]
    (api/create client tag)))

(defn import-swagger2 [client spec wsname]
  (or
   (some->> (common/find-existing-resource client (if (s/blank? wsname) (:title (:info spec)) wsname) #(api/map->Workspace {}))
            (:_id)
            (c/get-aggregated-workspace-by-id client)
            (update/update-workspace client spec))
    (let [model (common/find-or-create-model client "Swagger 2.0")
          _ (socket-send (str "Created Swagger2 mode\nStarting on workspace"))
          workspace (create-workspace client model wsname spec)
          _ (socket-send (str "Created workspace " (:name workspace) "\nAbout to create " (count (:paths spec)) " definitions"))
          defs (create-defs client model spec workspace)
          _ (socket-send (str "Created " (count defs) " definitions\nAbout to create " (count (:parameters spec)) " parameters"))
          params (create-params client model spec workspace)
          _ (socket-send (str "Created " (count params) " parameters\nAbout to create " (count (:securityDefinitions spec)) " security definitions"))
          secur (create-security-defs client model spec workspace)         
          _ (socket-send (str "Created " (count secur) " security definitions\nAbout to create " (count (:tags spec)) " tags"))
          tags-cache (atom (create-tags client spec (:_id workspace)))]
      (socket-send (str "Created " (count @tags-cache) " tags\nAbout to create " (count (:path spec)) "resources"))
      (create-resource client model spec defs params secur tags-cache workspace)
      (socket-send (str "Updating tags"))
      (update-tags client @tags-cache)
      (socket-send "Done importing swagger doc.")
      (str (:_id workspace)))))
