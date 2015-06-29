(ns ardoq.swagger.swagger-v2
  (:import [java.net URI URL])
  (:require
   [ardoq.swagger.client :as api]
   [clojurewerkz.urly.core :as urly]
   [org.httpkit.client :as http]
   [clostache.parser :as tpl]
   [clojure.java.io :as io]
   [cheshire.core :refer [generate-string parse-string]]
   [clojure.string :as s]
   [medley.core :refer [map-vals]]))

(defn- field-exists? [client field-name {:keys [_id] :as model}]
  (not (empty? (filter
                (fn [{:keys [name model]}]
                  (and (= name field-name)
                       (= model (str _id))))
                (api/find-all (api/map->Field {}) client)))))

(defn find-or-create-fields [client {model-id :_id :as model}]
  (when-not (field-exists? client "method" model)
    (-> (api/->Field "method" "method" "Text" (str model-id) [(api/type-id-by-name model "Operation")])
        (api/create client)))
  (when-not (field-exists? client "produces" model)
    (-> (api/->Field "produces" "produces" "List" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client)))
  (when-not (field-exists? client "consumes" model)
    (-> (api/->Field "consumes" "consumes" "List" (str model-id) [(api/type-id-by-name model "Operation") (api/type-id-by-name model "Resource")])
        (api/create client))))

(defn find-or-create-model [client]
  ;; Finds the model required for all details. If not found, creates a new one
  (if-let [model (first (filter #(= "Swagger 2.0" (:name %)) (api/find-all (api/map->Model {}) client)))]
    model
    (-> (api/map->Model (parse-string (slurp (io/resource "modelv2.json")) true))
        (api/create client))))
  
(defn create-workspace [title client {:keys [info] :as data}]
  ;; Creates a new workspace in the client. 
  (let [{:keys [_id]} (find-or-create-model client)
        name (or title (:title info))] 
    (-> (api/->Workspace name (tpl/render-resource "infoTemplate.tpl" (assoc info :workspaceName name)) _id)
        (assoc :views ["swimlane" "sequence" "integrations" "componenttree" "relationships" "tableview" "tagscape" "reader" "processflow"])
        (api/create client))))

(defn parse-info [spec result]
  ;;Copies the info data from spec into result
  (assoc result :info (:info spec)))

(defn parse-paths [spec result]
  ;;Copies the paths data from spec into result
  (assoc result :paths (:paths spec)))

(defn parse-definitions [spec result]
  ;;Copies the defenition data from spec into result
  (assoc result :definitions (:definitions spec)))

(defn parse-produces [spec result]
  ;;Copies the produces data from spec into result
  (assoc result :produces (:produces spec)))

(defn parse-consumes [spec result]
  ;;Copies the consumes data from spec into result
  (assoc result :consumes (:consumes spec)))

(defn parse-parameters [spec result]
  ;;Copies the parameters data from spec into result
  (assoc result :parameters (:parameters spec)))

(defn parse-security [spec result]
  ;;Copies the security data from spec into result
  (assoc result :security (:security spec)))

(defn parse-security-defs [spec result]
  ;;Copies the security definitions data from spec into result
  (assoc result :securityDefinitions (:securityDefinitions spec)))

(defn parse-tags [spec result]
  ;;Copies the tags data from spec into result
  (assoc result :tags (:tags spec)))


(defn create-tags [spec])

(defn model-template [m]
  (str "###JSON Schema\n```\n"
       (generate-string m {:pretty true})
       "\n```"))

(defn create-models [model wid _id path {:keys [definitions]}]
  ;;Creates links between components
  (reduce
      (fn [acc [type schema]]
        (assoc acc (keyword type)
               (assoc
                   (api/->Component type (model-template schema) (str wid) _id (api/type-id-by-name model "Model")  nil)
                 :schema schema)))
      {}
       definitions))

(defn update-comp [client component {:keys [produces consumes]}]
  ;; Updates a component based on previous modelling. Uses the swagger file to detect what it needs. 
  (api/update 
   (cond-> (api/map->Component component)
           produces (assoc :produces produces)
           consumes (assoc :consumes consumes)) client))

(defn generate-operation-description [data models]
  (reduce
   (fn [description [model-id {:keys [_id] :as model}]]
     (s/replace description (re-pattern (str "\\|" (name model-id) "\\|")) (str "|[" (name model-id) "](comp://" _id ")|")))
   (tpl/render-resource "operationTemplate.tpl" data)
   models))

(defn create-ops [client model models wid parent _id methods]
  (keep
   (fn [[method {parameters :parameters response :responses security :security :as data}]]
;     (clojure.pprint/pprint security)
     (if (not (= method (keyword "parameters")))
       (let [type (doall (map (fn [[_ v]]
                                (get-in v [:schema]))
                                     response))]
                (-> (api/map->Component {:name (name method) 
                                         :description (generate-operation-description data models) 
                                         :rootWorkspace (str wid) 
                                         :model _id 
                                         :parent (:_id parent) 
                                         :method method
                                         :typeId (api/type-id-by-name model "Operation")}) 
                    (api/create client) 
                    (assoc :return-model type
                           :input-models parameters
                           :security security)))))
   methods))

(defn create-methods [client model models wid _id path spec {:keys [component]} methods] 
  ;; Used to create all methods for the resources and links them with the parent
  (update-comp client component spec)
  (create-ops client model models wid component _id methods))

(defn save-models [models client]
  (map-vals #(let [schema (:schema %)]
               (assoc (api/create (dissoc % :schema) client) :schema schema)) models))

(defn find-nested-model-deps [model]
  ;;Finds all references in a given model
  (map (fn [v]
         (last (.split v "/")))
   (keep :$ref (tree-seq #(or (map? %) (vector? %)) identity model))))

(defn interdependent-model-refs [client models]
  ;;Creates refs between models
  (doall (mapcat
          (fn [model]
            (let [rrr (find-nested-model-deps model)]
              (doall (keep
                      (fn [model-key]
                        (if-let [m ((keyword model-key) models)]
                          (-> (api/map->Reference {:rootWorkspace (:rootWorkspace model)
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
                      (-> (api/map->Reference  {:rootWorkspace (:rootWorkspace comp)
                                                :source (str id)
                                                :target (str(:_id m))
                                                :type type})
                          (api/create client)))))
                keys))
    (let [ref (.split keys "/")
          k (keyword (last ref))]
      (if-let [m (k models)]
        (-> (api/map->Reference  {:rootWorkspace (:rootWorkspace comp)
                                  :source (str id)
                                  :target (str(:_id m))
                                  :type type})
            (api/create client))))))

(defn create-resource-refs [client {:keys [parameters] :as resource} params]
  (let [wid (get-in resource [:component :rootWorkspace])
        _id (get-in resource [:component :_id])]
    (doall (map
            (fn [{:keys [$ref]}]
              (let [k (keyword (last (.split $ref "/")))]
                (if-let [m (k params)]
                  (-> (api/map->Reference {:rootWorkspace wid
                                            :source (str _id)
                                            :target (str(:_id m))
                                            :type 1})
                      (api/create client)))))
            parameters))))


(defn create-refs [client operations models security]
  ;;Finds all $refs in operations and sends them to create-ref
  (concat (mapcat
           (fn [{input-models :input-models return-models :return-model secur :security id :_id :as comp}]
             (let [input-models (set input-models)
                   return-models (set return-models)
                   secur (set secur)]
               (let [input-refs 
                     (doall (keep (fn [k]
                                    (let [nest-key (find-nested-model-deps k)]
                                      (cond                                        
                                       (seq nest-key)
                                       (create-ref client nest-key models comp id 1)
                                       (get-in k [:type])
                                       (create-ref client (get-in k [:type]) models comp id 1)
                                       :else "nil")))
                                  input-models))])
               (doall (keep (fn [k]
                              (cond 
                               (seq (find-nested-model-deps k)) (create-ref client (find-nested-model-deps k) models comp id 0)
                               (get-in k [:$ref]) (create-ref client (get-in k [:$ref]) models comp id 0)
                               :else "nil"))
                            return-models))
               (doall (keep (fn [k]        
                              (if-let [m ((first (first k)) security)]
                                (-> (api/map->Reference  {:rootWorkspace (:rootWorkspace comp)
                                                          :source (str id)
                                                          :target (str(:_id m))
                                                          :type 1})
                                    (api/create client))))
                            secur))))
           operations)))

(defn create-defs [client {:keys [paths] :as spec}  workspace]
  ;; Creates the models
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-models model wid _id paths spec)
        (save-models client))))

(defn create-param-model [wid _id parameters description model]
  (reduce
   (fn [acc [param schema]]
     (assoc acc (keyword param)
            (assoc
                (api/->Component param (model-template schema) (str wid) _id (api/type-id-by-name model "Parameters") nil)
              :schema schema)))
   {}
   parameters))

(defn create-secur [model wid _id sec-defs description]
  (reduce
   (fn [acc [sec schema]]
     (assoc acc (keyword sec)
            (assoc
                (api/->Component sec (model-template schema) (str wid) _id (api/type-id-by-name model "securityDefinitions") nil)
              :schema schema)))
   {}
   sec-defs))


(defn create-security-defs [client {:keys [securityDefinitions] :as spec} workspace]
    (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
      (-> (create-secur model wid _id securityDefinitions description)
          (save-models client))))



(defn create-params [client {:keys [parameters] :as spec} workspace]
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-param-model wid _id parameters description model)
        (save-models client))))

(defn create-resource [client {:keys [paths definitions] :as spec} defs params secur workspace]
  ;;Create a resource. Does so by setting first path resource then adding the operations to it. Requires a full swagger file as input and the workspace it is being created in
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (doall
     (map (fn [[path {:keys [parameters] :as methods}]]
            (let [parent (doall {:resource path
                                 :parameters parameters
                                 :component (-> (api/->Component path description (str wid) _id (api/type-id-by-name model "Resource") nil)
                                                (api/create client))})
                  operations (create-methods client model defs wid _id path spec parent methods)]
              (create-resource-refs client parent params)
              (create-refs client operations defs secur)))
          paths))
    (interdependent-model-refs client defs)
    (find-or-create-fields client model)))

(defn get-info [client name spec]
  ;Converts the info from a Swagger 2 map to a string - This method needs to be redone
  (let [workspace (create-workspace name client spec)
        defs (create-defs client spec workspace)
        params (create-params client spec workspace)
        secur (create-security-defs client spec workspace)] 
    (create-resource client spec defs params secur workspace)))


(defn get-data [client spec name]
  ;;Extracts data from a given Swagger file into an emtpy object
   (->> {}
        (parse-info spec)
        (parse-paths spec)
        (parse-definitions spec)
        (parse-produces spec)
        (parse-consumes spec)
        (parse-parameters spec)
        (parse-security spec)
        (parse-security-defs spec)
        (parse-tags spec)
        (get-info client name)))

(defn import-swagger2 [client spec name]
  (get-data client spec name))



;; ISSUES
;; Fix the description for params and security-definitions
;; Do security properly, including how we handle them further in than root
;; Support ref in operation
;; Import tags
;; first first in create refs
