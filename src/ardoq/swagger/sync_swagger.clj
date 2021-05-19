(ns ardoq.swagger.sync-swagger
  (:require [ardoq.client :as api-client]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.map-openapi-3-spec :as map-openapi-3-spec]
            [ardoq.swagger.map-swagger-2-spec :as map-swagger-2-spec]
            [ardoq.swagger.model-utils :as model-utils]
            [ardoq.swagger.socket :refer [socket-send]]
            [ardoq.swagger.sync-overview-workspace :as sync-overview-workspace]
            [clojure.set :refer [difference]]))

(defn sync-component [client ardoq-data parent-component-id [spec-key spec-data-item] transformer-definition]
  (if-let [existing-component (get-in ardoq-data [:key->component spec-key])]
    (common/update-component client existing-component parent-component-id [spec-key spec-data-item])
    (common/create-component client ardoq-data parent-component-id [spec-key spec-data-item] transformer-definition)))


(defn sync-components [client ardoq-data spec-data transformer-definition]
  (reduce
    (fn [ardoq-components-by-spec-path [spec-key spec-data-item]]
      (let [parent-component-id (some->
                                  spec-data-item
                                  :parent
                                  ardoq-components-by-spec-path
                                  :_id)
            synced-ardoq-component
              (sync-component client ardoq-data parent-component-id [spec-key spec-data-item] transformer-definition)]
          (assoc ardoq-components-by-spec-path spec-key synced-ardoq-component)))
    {}
    (:swagger-object spec-data)))


(defn find-and-categorise-orphan-components [client ardoq-data ardoq-sync-data]
  (let [ardoq-components (:key->component ardoq-data)
        orphan-keys (difference (set (keys ardoq-components)) (set (keys ardoq-sync-data)))
        orphan-components (vals (select-keys ardoq-components orphan-keys))
        components-referencing-other-workspaces (:components-referencing-other-workspaces ardoq-data)]

    (group-by
      (fn [comp]
        (if
          (contains? components-referencing-other-workspaces (:_id comp))
          :to-mark-as-orphan
          :to-delete))
      orphan-components)))


(defn delete-components [client components]
  (doall
    (map
      (fn [orphan-component]
        (socket-send (str "Deleting Ardoq component " (:name orphan-component)))
        (try
          (api-client/delete (api-client/map->Component orphan-component) client)
          ;; is it better to delete from bottom and up?
          (catch Exception e (str "Exception deleting component. Probably because a parent was deleted before a child"))))
      components)))


(defn mark-as-orphans [client ardoq-data components transformer-definition]
  (doall
    (map
      (fn [orphan-component]
        (socket-send (str "Marking component " (:name orphan-component) " as orphan"))
        (let [orphan-type-name (get-in transformer-definition [:model-types :orphan])
              orphan-type-id (name (get-in ardoq-data [:model-name->type-id orphan-type-name]))]
          (-> orphan-component
            (assoc :description (common/render-resource-strings "templates/orphan-object.tpl" orphan-component))
            (assoc :parent nil)
            (assoc :open-api-path nil)
            (assoc :typeId orphan-type-id)
            (api-client/map->Component)
            (api-client/update client))))
      components)))


(defn map-to-ardoq-ids [ardoq-sync-components spec-data]
  (->>
    (:references spec-data)
    (map
      (fn [{source-path :source-path target-path :target-path }]
        {:source (:_id (ardoq-sync-components source-path))
         :target (:_id (ardoq-sync-components target-path))}))
    (filter
      (fn [{source :source target :target}]
        (and source target)))
    (into #{})))


(defn sync-references [client ardoq-data ardoq-sync-components spec-data transformer-definition]
  (let [spec-refs (map-to-ardoq-ids ardoq-sync-components spec-data)
        current-refs (set (keys (:key->reference ardoq-data)))
        new-refs (difference spec-refs current-refs)
        superfluous-refs (difference current-refs spec-refs)
        workspace-id (get-in ardoq-data [:workspace :_id])
        reference-type-name (:spec-reference-type-name transformer-definition)
        reference-type-id (model-utils/reference-type-id-from-name (:model ardoq-data) reference-type-name)]

    (doall (map (partial common/create-reference client workspace-id reference-type-id) new-refs))
    (doall (map (partial common/delete-reference client ardoq-data) superfluous-refs))))


(defn update-workspace-description [client ardoq-data spec transformer-definition]
  (let [description (apply (:workspace-description-fn transformer-definition) [spec])]
    (-> ardoq-data
        (:workspace)
        (assoc :description description)
        (api-client/update client))))



(defn sync-swagger [client spec wsname spec-version overview]
  (let [transformer-definition (cond
                      (= :openapi-3.x spec-version) map-openapi-3-spec/transformer-definition
                      (= :swagger-2.x spec-version) map-swagger-2-spec/transformer-definition)
        ardoq-data (or
                     (common/find-workspace-and-model client wsname transformer-definition)
                     (common/create-workspace-and-model client wsname spec transformer-definition))
        spec-data (apply (:transform-spec-fn transformer-definition) [spec])
        ardoq-sync-components (sync-components client ardoq-data spec-data transformer-definition)
        orphan-components (find-and-categorise-orphan-components client ardoq-data ardoq-sync-components)]

    (update-workspace-description client ardoq-data spec transformer-definition)
    (delete-components client (:to-delete orphan-components))
    (mark-as-orphans client ardoq-data (:to-mark-as-orphan orphan-components) transformer-definition)
    (sync-references client ardoq-data ardoq-sync-components spec-data transformer-definition)
    (socket-send "Done syncing specification")

    (let [spec-root-component (get ardoq-sync-components "#/")]
      (sync-overview-workspace/ensure-entry-in-overview-ws client spec-root-component overview))

    {:workspace-id (get-in ardoq-data [:workspace :_id])
     :root-component (get ardoq-sync-components "#/")}))


