(ns ardoq.swagger.common
  (:require [ardoq.swagger.client :as api]
            [cheshire.core :refer [generate-string parse-string]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojurewerkz.urly.core :as urly]
            [clostache.parser :as tpl]
            [medley.core :refer [map-vals]]
            [clj-http.client :as http]))

(defn replace-newlines [schema]
  (clojure.string/replace schema #"\\n" ""))

(defn find-or-create-model [client type]
  (if-let [model (first (filter #(= type (:name %)) (api/find-all (api/map->Model {}) client)))]
    model
    (-> (api/map->Model (parse-string (slurp (io/resource (if (= type = "Swagger") "modelv1.json" "modelv2.json"))) true))
        (api/create client))))

(defn find-existing-resource 
  ([client name type]
   (first (filter #(= name (:name %)) (api/find-all (type) client))))
  ([client name type root-id]
   (first (filter #(= name (:name %)) (api/find-in-workspace (type) client root-id)))))

(defn set-id-and-version [{id :_id version :_version} resource]
  (println id)
  (println version)
  (println resource)
  (assoc resource :_id id :_version version))

(defn- field-exists? [client field-name {:keys [_id] :as model}]
  (seq (filter
        (fn [{:keys [name model]}]
          (and (= name field-name)
               (= model (str _id))))
        (api/find-all (api/map->Field {}) client))))

(defn find-or-create-fields [client {model-id :_id :as model}]
  (when-not (field-exists? client "method" model)
    (-> (api/->Field "method" "method" "Text" (str model-id) [(api/type-id-by-name model "Operation")])
        (api/create client)))
  (when-not (field-exists? client "produces" model)
    (-> (api/->Field "produces" "produces" "Text" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client)))
  (when-not (field-exists? client "consumes" model)
    (-> (api/->Field "consumes" "consumes" "Text" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client))))

(defn generate-operation-description [data models]
  (let [data (read-string (replace-newlines data))]
    (reduce
     (fn [description [model-id {:keys [_id] :as model}]]
       (s/replace description (re-pattern (str "\\|" (name model-id) "\\|")) (str "|[" (name model-id) "](comp://" _id ")|")))
     (tpl/render-resource "operationTemplate.tpl" data)
     models)))

(defn update-comp [client component {:keys [produces consumes]}]
  ;; Updates a component based on previous modelling. Uses the swagger file to detect what it needs. 
  (api/update 
   (cond-> (api/map->Component component)
     produces (assoc :produces produces)
     consumes (assoc :consumes consumes)) client))

(defn save-models [models client]
  (map-vals #(let [schema (:schema %)]
               (or (some-> 
                    (find-existing-resource client (name (:name %)) (partial api/map->Component {}) (:rootWorkspace %))
                    (set-id-and-version %)
                    (dissoc :schema)
                    (api/update client)
                    (assoc :schema schema)))
               (assoc (api/create (dissoc % :schema) client) :schema schema)) models))
