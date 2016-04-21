(ns ardoq.swagger.swagger2-refs
  (:require [ardoq.implement.api :as api]
            [ardoq.core :as c]
            [ardoq.swagger.common :as common]))


(defn find-nested-model-deps [model]
  ;;Finds all references in a given model
  (doall (map (fn [v]
                (if (instance? String v)
                  (last (.split v "/"))
                  ""))
              (doall (keep :$ref (tree-seq #(or (map? %) (vector? %)) identity model))))))

(defn interdependent-model-refs [client models]
  ;;Creates refs between models
  (doall (mapcat
          (fn [model]
            (let [rrr (find-nested-model-deps model)]
              (doall (keep
                      (fn [model-key]
                        (if-let [m ((keyword model-key) models)]
                          (->> (api/map->Reference {:name ""
                                                    :description ""
                                                    :rootWorkspace (:rootWorkspace model)
                                                    :source (str (:_id model))
                                                    :target (str (:_id m))
                                                    :type 3})
                               (api/create client))))
                      rrr))))
          (vals models))))

(defn create-ref [client keys models comp id type]
  ;; Makes the refs given the values found by create-refs
  (if (seq? keys)
    (doall (map (fn [ref]
                  (let [ref (.split ref "/")
                        k (keyword (last ref))]
                    (if-let [m (k models)]
                      (->> (api/map->Reference  {:name ""
                                                 :description ""
                                                 :rootWorkspace (:rootWorkspace comp)
                                                 :source (str id)
                                                 :target (str(:_id m))
                                                 :type type})
                           (api/create client)))))
                keys))
    (let [ref (.split keys "/")
          k (keyword (last ref))]
      (if-let [m (k models)]
        (->> (api/map->Reference  {:name ""
                                   :description ""
                                   :rootWorkspace (:rootWorkspace comp)
                                   :source (str id)
                                   :target (str(:_id m))
                                   :type type})
             (api/create client))))))

(defn create-resource-refs [client {:keys [parameters] :as resource} params]
  (let [wid (get-in resource [:component :rootWorkspace])
        _id (get-in resource [:component :_id])]
    (doseq [{:keys [$ref]} parameters]
      (let [k (keyword (last (.split $ref "/")))]
        (if-let [m (k params)]          
          (c/create-reference client "" "" wid (str _id) (str (:_id m)) 1))))))

(defn create-input-refs [client input-models models comp id]
  (doall (keep (fn [k]
                 (let [nest-key (find-nested-model-deps k)]
                   (cond
                    (seq nest-key)
                    (create-ref client nest-key models comp id 1)
                    (get-in k [:type])
                    (create-ref client (get-in k [:type]) models comp id 1)
                    :else "nil")))
               input-models)))

(defn create-return-refs [client return-models models comp id]
  (doall (keep (fn [k]
                 (cond 
                  (seq (find-nested-model-deps k)) (create-ref client (find-nested-model-deps k) models comp id 0)
                  (get-in k [:$ref]) (create-ref client (get-in k [:$ref]) models comp id 0)
                  :else "nil"))
               return-models)))

(defn create-security-refs [client securities security models comp id]
  (doall (keep (fn [k]        
                 (if-let [m ((first (first k)) security)]
                   (->> (api/map->Reference  {:name ""
                                              :description ""
                                              :rootWorkspace (:rootWorkspace comp)
                                              :source (str id)
                                              :target (str(:_id m))
                                              :type 1})
                        (api/create client))))
               securities)))

(defn create-refs [client operations models security]
  ;;Finds all $refs in operations and sends them to create-ref
  (mapcat
   (fn [{input-models :input-models return-models :return-model securities :security id :_id :as comp}]
     (let [input-models (set input-models)
           return-models (set return-models)
           securities (set securities)]
       (create-input-refs client input-models models comp id)
       (create-return-refs client return-models models comp id)
       (create-security-refs client securities security models comp id)))
   operations))
