(ns ardoq.swagger.update-swagger2
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]))

(defn get-component-by-type [workspace type]
  (doall (filter #(= type (:type %)) (:components workspace))))

(defn create-component [client type schema {wid :_id} {_id :_id :as model} type-name]
  (-> (assoc nil (keyword type)
             (assoc
                 (api/->Component type (common/model-template schema) (str wid) _id (api/type-id-by-name model type-name)  nil)
               :schema schema))
      (common/save-models client)))

;;This function might not be possible as a single function. If so split into many and use partial
(defn update-component [client workspace component]
  "random return")

(defn update-components [client components definitions workspace {_id :_id :as model} model-type]
  (doseq [{def-name :name :as component} components]
    (when-not (first (filter #(= (name %) def-name) (keys definitions)))
      (api/delete (api/map->Component component) client)))
  (reduce (fn [acc [def-name data]]
            (assoc acc (keyword def-name) 
                   (or (some->> (first (filter #(= (name def-name) (:name %)) components))
                                (update-component client workspace))
                       (first (vals (create-component client def-name data workspace model model-type))))))
          {}
          definitions))

(defn update-methods [client]
  "random return"
  )

(defn add-new-operation [client method workspace]
  (println (str "adding " method))
  "random return"
  )

(defn update-operation [client method resource]
  ;;The path/resource operation itself has no true values, we just keep it as is
  ;;But the internal are different in regards to methods it has. 
  ;;However these are connected by parent in the resource
  ;;Only question remaining is if the consume or produces need changing
  "Random return")

(defn update-operations [client resources {paths :paths :as spec} workspace]
  ;;This runs two times doseq filter. 
  (doseq [{path :name :as resource} resources]
    (when-not (first (filter #(= (name %) path) (keys paths)))
      (api/delete (api/map->Component resource) client)))
  (doseq [[path :as method] paths]
    (or (some->> (first (filter #(= (name path) (:name %)) resources))
                (update-operation client method))
        (add-new-operation client method workspace))))

(defn update-workspace [workspace client spec]
  ;;Workspace exists, we will just update values in it.
  ;;might need to update through the infoTemplate (see swagger-v2 line 17
  ;(clojure.pprint/pprint (:components workspace))
  (let [model (common/find-or-create-model client "Swagger 2.0")
        defs (update-components client (get-component-by-type workspace "Model")  (:definitions spec) workspace model "Model")
        params (update-components client (get-component-by-type workspace "Parameters")  (:Parameters spec) workspace model "Parameters") ;TODO Been unable to find swagger with params to test
        securs (update-components client (get-component-by-type workspace "securityDefinitions") (:securityDefinitions spec) workspace model "securityDefinitions")
        tags {}] 
    )
  (update-operations client (get-component-by-type workspace "Resource") spec workspace)
  ;;update tags
  "Random return thats not nil")
