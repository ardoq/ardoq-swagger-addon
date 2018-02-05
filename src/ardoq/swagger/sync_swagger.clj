(ns ardoq.swagger.sync-swagger
  (:require [ardoq.swagger.client :as api-client]
            [ardoq.swagger.common :as common]
            [ardoq.swagger.model-utils :as model-utils]
            [ardoq.swagger.socket :refer [socket-send]]
            [ardoq.swagger.map-openapi-3-spec :as map-openapi-3-spec]
            [ardoq.swagger.map-openapi-3-spec :as map-swagger-2-spec]
            [cheshire.core :refer [generate-string parse-string]]
            [org.httpkit.server :as srv]
            [flatland.ordered.map :as maps]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [difference]]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]))



(defn create-component [client ardoq-data parent-component-id [spec-key spec-data-item] transformer-definition]
  (let [type-key (:type spec-data-item)
        ardoq-type-name (get-in transformer-definition [:model-types type-key])
        ardoq-type-id (name (get-in ardoq-data [:model-name->type-id ardoq-type-name]))]
    (->
      spec-data-item
      (select-keys [:name :description])
      (assoc :typeId ardoq-type-id)
      (assoc :parent parent-component-id)
      (assoc :open-api-path spec-key)
      (assoc :rootWorkspace (get-in ardoq-data [:workspace :_id]))
      (assoc :model (get-in ardoq-data [:model :_id]))
      (api-client/map->Component)
      (api-client/create  client))))


(defn update-component [client existing-component parent-component-id [spec-key spec-data-item]]
  (let [updated-component (-> existing-component
                              (merge (select-keys spec-data-item [:name :description]))
                              (assoc :parent parent-component-id)
                              (assoc :open-api-path spec-key))]
    (if (= updated-component existing-component)
      (api-client/map->Component existing-component)
      (api-client/update (api-client/map->Component updated-component) client))))


(defn sync-component [client ardoq-data parent-component-id [spec-key spec-data-item] transformer-definition]
  (if-let [existing-component (get-in ardoq-data [:key->component spec-key])]
    (update-component client existing-component parent-component-id [spec-key spec-data-item])
    (create-component client ardoq-data parent-component-id [spec-key spec-data-item] transformer-definition)))


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
          (prn "Syncing " spec-key (:_id synced-ardoq-component))
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
        (prn "deleting " (:name orphan-component) (:_id orphan-component))
        (try
          (api-client/delete (api-client/map->Component orphan-component) client)
          ;; is it better to delete from bottom and up?
          (catch Exception e (str "Exception deleting component. Probably because a parent was deleted before a child"))))
      components)))


(defn mark-as-orphans [client ardoq-data components transformer-definition]
  (doall
    (map
      (fn [orphan-component]
        (prn "changing type of " (:name orphan-component) (:_id orphan-component))
        (let [orphan-type-name (get-in transformer-definition [:model-types :Orphan])
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


(defn create-reference [client ardoq-data {source :source target :target}]
  (prn "Creating reference from" source "to" target)

  (-> {
        :source source
        :target target
        :type 0
        :rootWorkspace (get-in ardoq-data [:workspace :_id])
        :targetWorkspace (get-in ardoq-data [:workspace :_id])}
     (api-client/map->Reference)
     (api-client/create client)))


(defn delete-reference [client ardoq-data ref-key]
  (prn "Deleting reference from" (:source ref-key) "to" (:target ref-key))

  (-> (get-in ardoq-data [:key->reference ref-key])
    (api-client/map->Reference)
    (api-client/delete client)))


(defn sync-references [client ardoq-data ardoq-sync-components spec-data]
  (let [spec-refs (map-to-ardoq-ids ardoq-sync-components spec-data)
        current-refs (set (keys (:key->reference ardoq-data)))
        new-refs (difference spec-refs current-refs)
        superfluous-refs (difference current-refs spec-refs)]

    (doall (map (partial create-reference client ardoq-data) new-refs))
    (doall (map (partial delete-reference client ardoq-data) superfluous-refs))))


(defn sync-swagger [client spec wsname spec-version]
  (let [transformer-definition (cond
                      (= :openapi-3.x spec-version) map-openapi-3-spec/transformer-definition
                      (= :swagger-2.x spec-version) map-swagger-2-spec/transformer-definition)
        ardoq-data (or
                     (common/find-workspace-and-model client wsname transformer-definition)
                     (common/create-workspace-and-model client wsname spec transformer-definition))
        spec-data (apply (:transform-spec-fn transformer-definition) [spec])
        ardoq-sync-components (sync-components client ardoq-data spec-data transformer-definition)
        orphan-components (find-and-categorise-orphan-components client ardoq-data ardoq-sync-components)]

    (delete-components client (:to-delete orphan-components))
    (mark-as-orphans client ardoq-data (:to-mark-as-orphan orphan-components) transformer-definition)
    (sync-references client ardoq-data ardoq-sync-components spec-data)

    (prn "synced")

  ))



