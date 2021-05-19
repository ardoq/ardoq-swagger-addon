(ns ardoq.swagger.sync-overview-workspace
  (:require [ardoq.client :as api-client]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.model-utils :as model-utils]))

(def implicit-reference-type-id 2)

(defn create-overview-component [client spec-root-component workspace entry-type-name reference-type-name]
  (let [model-id (:componentModel workspace)
        model (-> {:_id model-id}
                  (api-client/map->Model)
                  (api-client/find-by-id client))
        type-id (model-utils/type-id-by-name model entry-type-name)]
    (if type-id
      (let [component (->
                        spec-root-component
                        (select-keys [:name :description])
                        (assoc :typeId type-id)
                        (assoc :rootWorkspace (:_id workspace))
                        (assoc :model model-id)
                        (assoc :lock (get-in client [:user :_id]))
                        (api-client/map->Component)
                        (api-client/create client))
            reference-type-id (:id (second (first (filter (fn [[k v]] (= (:name v) reference-type-name)) (:referenceTypes model)))))]
        (common/create-reference
            client
            (:_id workspace)
            (or reference-type-id implicit-reference-type-id)
            {:source (:_id component)
             :target (:_id spec-root-component)})
        (prn "Component created in overview workspace"))
      (prn (str "Type \"" entry-type-name "\" not found in workspace \"" (:name workspace) "\"")))))

(defn update-overview-component [client spec-root-component existing-component]
  (-> existing-component
      (assoc :description (:description spec-root-component))
      (api-client/update client)))


(defn ensure-entry-in-overview-ws [client spec-root-component overview]
  (when-let [workspace (common/find-existing-resource client (:overview-workspace overview) #(api-client/map->Workspace {}))]
    (if-let [existing-component (->> (:name spec-root-component)
                                  (api-client/find-components-by-name client (:_id workspace))
                                  (filter #(= (:type %) (:overview-component-type overview)))
                                  first)]
      (update-overview-component client spec-root-component existing-component)
      (create-overview-component client spec-root-component workspace (:overview-component-type overview) (:overview-reference-type overview)))))
