(ns ardoq.swagger.update-swagger2
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.swagger2-refs :as refs]))

(defn get-component-by-type [workspace type]
  (doall (filter #(= type (:type %)) (:components workspace))))

(defn create-component [client type schema {wid :_id} {_id :_id :as model} type-name template]
  (-> (assoc nil (keyword type)
             (assoc
                 (api/->Component type (template schema) (str wid) _id (api/type-id-by-name model type-name)  nil)
               :schema schema))
      (common/save-models client)))

(defn update-component [client component template data]
  (-> (assoc data :description (template component))
      (api/map->Component)
      (api/update client)))

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

(defn create-or-update-operation [client {parent :component} {_id :_id :as model} wid path methods tags defs resource components]
  (doseq [child (:children parent)]
    ;;We need to find if the name of the children parent and check if they fit with path+method
    ;;If we got a child thats ont in the path+method list it should get deleted.
    ;;Something link filtering the list of childs towards components then checking the name of each towards methods.
    "This"
    )
  (doall (keep
          (fn [[method-name method]]
            (if (not (= method-name (keyword "parameters")))
              (or (some-> (first (filter #(and 
                                           (= (:parent %) (:_id resource)) 
                                           (= (str (:name resource) "/" (name method-name)) (:name %))) 
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
                                           :typeId (api/type-id-by-name model "Operation")})
                      (api/create client)
                      (assoc :return-model (doall (map 
                                                   (fn [[_ v]] (get-in v [:schema]))
                                                   (:responses method)))
                             :input-models (:parameters method)
                             :security (:security method))))))
          methods)))

(defn create-method [client [path methods] {wid :_id :as workspace} params {_id :_id :as model} tags defs securs]
  (let [description (:description (first (vals methods)))
        parameters (:parameters (first (vals methods)))
        parent {:resource path
                :paramters parameters
                :component (-> (api/->Component path (or description "") (str wid) _id (api/type-id-by-name model "Resource") nil)
                    (api/create client))}
        op (create-or-update-operation client parent model wid path methods tags defs nil nil)]
    (refs/create-resource-refs client parent params)
    (refs/create-refs client op defs securs)
    parent))

(defn update-operation [client [path methods] {wid :_id :as workspace} params {_id :_id :as model} tags defs securs resource]
  ;;The path/resource operation itself has no true values, we just keep it as is
  ;;But the internal are different in regards to methods it has. 
  ;;However these are connected by parent in the resource
  ;;Only question remaining is if the consume or produces need changing
  
  ;;Things that might change - responses, parameters, produces, tags, references
  (let [description (:description (first (vals methods)))
        parameters (:parameters (first (vals methods)))
        parent {:resource path
                :paramters parameters
                :component (-> (api/map->Component resource)                          
                               (api/update client))}
        ;;Create or update op here
        op (create-or-update-operation client parent model wid path methods tags defs resource (:components workspace))]
    (refs/create-resource-refs client parent params)
    (refs/create-refs client op defs securs)
    parent))

(defn update-operations [client resources {paths :paths :as spec} workspace model defs params securs tags]
  ;;This runs two times doseq filter. 
  (doseq [{path :name :as resource} resources]
    (when-not (first (filter #(= (name %) path) (keys paths)))
      (api/delete (api/map->Component resource) client)))
  (reduce (fn [acc [def-name data :as component]]        
            (assoc acc (keyword def-name) 
                   (or (some->> (first (filter #(= (name def-name) (:name %)) resources))
                                (update-operation client component workspace params model tags defs securs))
                       (first (vals (create-method client component workspace params model tags defs securs))))))
          {}
          paths)
  (refs/interdependent-model-refs client defs) ;;This doesn't happen for some reason
  (common/find-or-create-fields client model))

(defn delete-references [client {references :references :as workspace}]
  (doseq [ref references]
    (when (= (:rootWorkspace ref) (:targetWorkspace ref) (:_id workspace))
      (api/delete (api/map->Reference ref) client))))

(defn update-workspace [workspace client spec]
  ;;Workspace exists, we will just update values in it and potentially the description.
;  (clojure.pprint/pprint workspace)
  (let [model (common/find-or-create-model client "Swagger 2.0")
        defs (update-components client (get-component-by-type workspace "Model")  (:definitions spec) workspace model "Model" (partial common/model-template))
        params (update-components client (get-component-by-type workspace "Parameters")  (:Parameters spec) workspace model "Parameters" (partial common/generate-param-description)) ;TODO Been unable to find swagger with params to test
        securs (update-components client (get-component-by-type workspace "securityDefinitions") (:securityDefinitions spec) workspace model "securityDefinitions" (partial common/generate-security-description))
        tags {}]     
    (delete-references client workspace)
    (update-operations client (get-component-by-type workspace "Resource") spec workspace model defs params securs tags))
  ;;update tags
  (println "DONE")
  "Random return thats not nil")
