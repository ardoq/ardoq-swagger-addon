(ns ardoq.swagger.update-swagger2
  (:require [ardoq.swagger.client :as api]
            [ardoq.swagger.common :as common]
            [clojure.data :as d]))


(defn add-new-operation [client method workspace]
  (println (str "adding " method))
  "random return"
  )

(defn remove-operation [client method workspace]
  (println (str "removing" method))
  "random return")

(defn update-operation [resource client method]
  ;;The path/resource operation itself has no true values, we just keep it as is
  ;;But the internal are different in regards to methods it has. 
  ;;However these are connected by parent in the resource
  ;;Only question remaining is if the consume or produces need changing
  "Random return")

(defn update-operations [client resources {paths :paths :as spec} workspace]
  (doseq [[path :as method] paths]
    (or (some-> (first (filter #(= (name path) (:name %)) resources))
                (update-operation client method))
        (add-new-operation client method workspace)))
  (doseq [{path :name :as method} resources]
    (println "Next one")
    (when-not (first (filter #(= (name %) path) (keys paths)))                
      (remove-operation client method workspace))))

(defn update-workspace [workspace client spec]
  ;;Workspace exists, we will just update values in it.
  ;(clojure.pprint/pprint workspace)
  (update-operations client (doall (filter #(= "Resource" (:type %)) (:components workspace))) spec workspace)
  "Random return thats not nil")
