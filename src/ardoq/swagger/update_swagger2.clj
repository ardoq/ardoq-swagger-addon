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

(defn update-references [client operation]
  "random return")

(defn create-operation [client {parent :component} {_id :_id :as model} wid path methods tags defs]
  (doall (keep
          (fn [[method-name method]]
            (if (not (= method-name (keyword "parameters")))
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
                         :security (:security method)))))
          methods)))

(defn create-method [client [path methods] {wid :_id :as workspace} params {_id :_id :as model} tags params defs securs]
  (let [description (:description (first (vals methods)))
        parameters (:parameters (first (vals methods)))
        parent {:resource path
                :paramters parameters
                :component (-> (api/->Component path (or description "") (str wid) _id (api/type-id-by-name model "Resource") nil)
                    (api/create client))}
        op (create-operation client parent model wid path methods tags defs)]
    (refs/create-resource-refs client parent params)
    (refs/create-refs client op defs securs)
    ;;Should create the sub operations straight away. We know they don't exist anyway
    ;;And all references we need seeing how they all come from within or parameters
    ;;Those that don't get created seperately
    parent))

(defn update-operation [client method resource]
  ;;The path/resource operation itself has no true values, we just keep it as is
  ;;But the internal are different in regards to methods it has. 
  ;;However these are connected by parent in the resource
  ;;Only question remaining is if the consume or produces need changing
  
  ;;Things that might change - responses, parameters, produces, tags, references


  ;(common/update-comp client resource spec)
  
  "Random return")

(defn update-operations [client resources {paths :paths :as spec} workspace model defs params securs tags]
  ;;This runs two times doseq filter. 
  (doseq [{path :name :as resource} resources]
    (when-not (first (filter #(= (name %) path) (keys paths)))
      (api/delete (api/map->Component resource) client)))
  (reduce (fn [acc [def-name data :as component]]        
            (assoc acc (keyword def-name) 
                   (or (some->> (first (filter #(= (name def-name) (:name %)) resources))
                                (update-operation client (first (rest component))))
                       (first (vals (create-method client component workspace params model tags params defs securs))))))
          {}
          paths)
  (refs/interdependent-model-refs client defs)
  (common/find-or-create-fields client model);;THIS MIGHT BE A BIT EXCESSIVE
  )

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
