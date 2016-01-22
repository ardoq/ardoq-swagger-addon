(ns ardoq.swagger.update-swagger2
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]))

(defn create-def [client type schema {wid :_id} {_id :_id :as model}]
  (-> (assoc nil (keyword type)
             (assoc
                 (api/->Component type (common/model-template schema) (str wid) _id (api/type-id-by-name model "Model")  nil)
               :schema schema))
      (common/save-models client)))


(defn update-defs [client models {definitions :definitions :as spec} workspace model]
  (doseq [{def-name :name :as model} models]
    (when-not (first (filter #(= (name %) def-name) (keys definitions)))
      (api/delete (api/map->Component model) client)))
  (reduce (fn [acc [def-name data]]
            (assoc acc (keyword def-name) 
                   (or (some->> (first (filter #(= (name def-name) (:name %)) models))
                                (str "TO BE UPDATED: "))
                       (first (vals (create-def client def-name data workspace model))))))
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
  (doseq [[path :as method] paths]
    (or (some->> (first (filter #(= (name path) (:name %)) resources))
                (update-operation client method))
        (add-new-operation client method workspace)))
  (doseq [{path :name :as resource} resources]
    (when-not (first (filter #(= (name %) path) (keys paths)))
      (api/delete (api/map->Component resource) client))))

(defn update-workspace [workspace client spec]
  ;;Workspace exists, we will just update values in it.
  ;;might need to update through the infoTemplate (see swagger-v2 line 17
  ;(clojure.pprint/pprint (:components workspace))
  (let [model (common/find-or-create-model client "Swagger 2.0")]
    (clojure.pprint/pprint (update-defs client (doall (filter #(= "Model" (:type %)) (:components workspace))) spec workspace model)))
  ;;check params
  ;;check securs
  ;;check tags
  (update-operations client (doall (filter #(= "Resource" (:type %)) (:components workspace))) spec workspace)
  "Random return thats not nil")
