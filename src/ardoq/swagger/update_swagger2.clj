(ns ardoq.swagger.update-swagger2
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.socket :refer [socket-send]]
            [ardoq.swagger.model-utils :as model-utils]
            [org.httpkit.server :as srv]
            [ardoq.swagger.swagger2-refs :as refs]))

(defn get-component-by-type [workspace type]
  (doall (filter #(= type (:type %)) (:components workspace))))

(defn create-component [client type schema {wid :_id} {_id :_id :as model} type-name template]
  (-> (assoc nil (keyword type)
             (assoc
                 (api/->Component type (template schema) (str wid) _id (model-utils/type-id-by-name model type-name)  nil)
               :schema schema))
      (common/save-models client nil)))

(defn update-component [client component template data]
  (-> (assoc data :description (template component))
      (api/map->Component)
      (api/update client)
      (assoc :schema component)))

(defn update-components [client components definitions workspace {_id :_id :as model} model-type template]
  (doseq [{def-name :name :as component} components]
    (when-not (first (filter #(= (name %) def-name) (keys definitions)))
      (api/delete (api/map->Component component) client)))
  (reduce (fn [acc [def-name data :as component]]        
            (assoc acc (keyword def-name) 
                   (or (some->> (first (filter #(= (name def-name) (:name %)) components))
                                (update-component client (first (rest component)) template))
                       (first (vals (create-component client def-name data workspace model model-type template))))))
          {}
          definitions))

(defn create-or-update-operation [client {parent :component} {_id :_id :as model} wid path methods tags defs components spec]
  (common/update-comp client parent spec)
  (doseq [child (:children parent)]
    (let [resource (first (filter #(= (:_id %) child) components))]
      (when-not (first (filter #(= (str (name path) "/" (name %)) (:name resource)) (keys methods)))
        (api/delete (api/map->Component resource) client))))
  (doall (map
          (fn [[method-name method]]
            (if (not (= method-name (keyword "parameters")))
              (let [op (or (some-> (first (filter #(and 
                                                    (= (:parent %) (:_id parent)) 
                                                    (= (str (:name parent) "/" (name method-name)) (:name %))) 
                                                  components))
                                   (assoc :description (common/generate-operation-description method defs))
                                   (api/map->Component)
                                   (api/update client)
                                   (assoc :return-model (doall (map 
                                                                (fn [[_ v]] (get-in v [:schema]))
                                                                (:responses method)))
                                          :input-models (:parameters method)
                                          :security (:security method)))
                           (-> (api/map->Component {:name (str (name path) "/" (name method-name))
                                                    :description (common/generate-operation-description method defs)
                                                    :rootWorkspace (str wid)
                                                    :model _id
                                                    :parent (:_id parent)
                                                    :method method-name
                                                    :typeId (model-utils/type-id-by-name model "Operation")})
                               (api/create client)
                               (assoc :return-model (doall (map 
                                                            (fn [[_ v]] (get-in v [:schema]))
                                                            (:responses method)))
                                      :input-models (:parameters method)
                                      :security (:security method))))]
                (common/find-or-create-tag client (:tags method) wid op tags)
                op)))
          methods)))

(defn create-method [client [path methods] {wid :_id :as workspace} params {_id :_id :as model} tags defs securs spec]
  (let [description (:description (first (vals methods)))
        parameters (:parameters methods)
        parent {:resource path
                :parameters parameters
                :component (-> (api/->Component path (or description "") (str wid) _id (model-utils/type-id-by-name model "Resource") nil)
                    (api/create client))}

        op (create-or-update-operation client parent model wid path methods tags defs nil spec)]
    (refs/create-resource-refs client parent params)
    (refs/create-refs client op defs securs)
    parent))

(defn update-operation [client [path methods] {wid :_id :as workspace} params {_id :_id :as model} tags defs securs spec resource]
  ;;The path/resource operation itself has no true values, we just keep it as is
  ;;But the internal are different in regards to methods it has. 
  ;;However these are connected by parent in the resource
  (let [description (:description (first (vals methods)))
        parameters (:parameters methods)
        parent {:resource path
                :parameters parameters
                :component (-> (api/map->Component resource)                          
                               (api/update client))}
        ;;Create or update op here
        op (create-or-update-operation client parent model wid path methods tags defs (:components workspace) spec)]
    (refs/create-resource-refs client parent params)
    (refs/create-refs client op defs securs)
    parent))

(defn update-operations [client resources {paths :paths :as spec} workspace model defs params securs tags]
  ;;This runs two times doseq filter. 
  (doseq [{path :name :as resource} resources]
    (when-not (first (filter #(= (name %) path) (keys paths)))
      (api/delete (api/map->Component resource) client)))
  (reduce (fn [acc [def-name data :as component]]        
            (socket-send (str "Updating operation " (name def-name)))
            (assoc acc (keyword def-name) 
                   (or (some->> (first (filter #(= (name def-name) (:name %)) resources))
                                (update-operation client component workspace params model tags defs securs spec))
                       (first (vals (create-method client component workspace params model tags defs securs spec))))))
          
          {}
          paths)
  (refs/interdependent-model-refs client defs) ;;This doesn't happen for some reason
  (common/find-or-create-fields client model))

(defn collect-tags [client {wtags :tags wid :_id :as workspace} tags]
  (doseq [{tag-name :name :as wtag} wtags]
    (when-not (first (filter #(= (:name %) tag-name) tags))
      (api/delete (api/map->Tag wtag) client)))
  (doall (reduce
          (fn [acc {name :name description :description :as tag}]
            (assoc acc name
                   (or (some-> (first (filter #(= (:name tag) (:name %)) wtags))
                               (assoc :name name :description description))
                       (api/->Tag name description wid [] []))))
          {}
          tags)))

(defn update-tags [client tags {wid :_id}]
  (doseq [[_ tag] tags] 
    (if (:_version tag)
      (-> (assoc tag :_version (:_version (api/find-by-id (api/map->Tag tag) client)))
          (api/map->Tag)
          (api/update client))
      (api/create (api/map->Tag tag) client))))

(defn delete-references [client {references :references :as workspace}]
  (doseq [ref references]
    (when (= (:rootWorkspace ref) (:targetWorkspace ref) (:_id workspace))
      (api/delete (api/map->Reference ref) client))))

(defn update-workspace [workspace client spec]
  (socket-send (str "Updating workspace " (:name workspace)))
  ;;Workspace exists, we will just update values in it and potentially the description.
  (let [model (api/find-by-id (:componentModel workspace) client)
        _ (socket-send (str "Found Swagger 2 model""\nGonna update " (count (:paths spec)) " defitinitions"))
        defs (update-components client (get-component-by-type workspace "Model")  (:definitions spec) workspace model "Model" (partial common/model-template))
        _ (socket-send (str "Updated " (count defs) " definitions\nGonna update " (count (:parameters spec)) " parameters"))
        params (update-components client (get-component-by-type workspace "Parameters")  (:parameters spec) workspace model "Parameters" (partial common/generate-param-description))
        _ (socket-send (str "Updated " (count params) " parameters\nGonna update " (count (:securityDefinitions spec)) " security definitions"))
        securs (update-components client (get-component-by-type workspace "securityDefinitions") (:securityDefinitions spec) workspace model "securityDefinitions" (partial common/generate-security-description))
        _ (socket-send (str "Updated " (count securs) " security definitions\nGonna update " (count (:tags spec)) " tags"))
        tags (atom (collect-tags client workspace (:tags spec)))
        _ (socket-send (str "Updated  " (count @tags) " tags\nPreparing references"))
        workspace (api/find-aggregated workspace client)]
    (delete-references client workspace)
    (socket-send (str "Deleted " (count (:references workspace))  " internal references\nUpdating operations"))
    (update-operations client (get-component-by-type workspace "Resource") spec workspace model defs params securs tags)
    (socket-send (str "Updated " "operations"))
    (update-tags client @tags workspace))
  (str (:_id workspace)))
