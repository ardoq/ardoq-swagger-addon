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

(defn model-template [m]
  (str "###JSON Schema\n```\n"
       (generate-string m {:pretty true})
       "\n```"))

(defn generate-param-description [data]
  (-> (replace-newlines data)
      (tpl/render-resource "globalTemplate.tpl" data)))

(defn generate-security-description[data]
  (tpl/render-resource "securityTemplate.tpl" data))

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
           produces (-> ;This gets the cond macro
                     (assoc :produces produces)
                     (dissoc :consumes))
           consumes (-> ;This gets the cond macro
                     (assoc :consumes consumes)
                     (dissoc :produces))) client))

(defn save-models [models client]
  (map-vals #(let [schema (:schema %)]
               (assoc (api/create (dissoc % :schema) client) :schema schema)) models))


(defn find-or-create-tag [client tag wid op tags]
  (doall (map (fn [name]
                ;;Check if the tag exists, otherwise we create it
                (if (not (clojure.string/blank? name))
                  (if (get (deref tags) name)
                    (when-not (some #{(:_id op)} (get-in @tags [name :components]))
                      (swap! tags (fn [old]
                                    (update-in old [name :components] conj (get-in op [:_id])))))
                    (swap! tags (fn [old]
                                  (assoc old name (api/->Tag name "" wid [(get-in op [:_id])] [])))))))
              tag)))
