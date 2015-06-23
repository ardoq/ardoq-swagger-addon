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

;;This is a test cleint to ease implementation. Delete upon completion
(def client (api/client {:url "http://127.0.0.1:8080"
                       :token "9b2a9517e5c540a791f9db2468866a4f"
                       :org "ardoq"}))

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
    (-> (api/map->Model (parse-string (slurp (io/resource "model.json")) true))
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

(defn contact-str [{:keys [:contact]}]
  ;;Converts the contact part of the info to a markdown table. Needs fixing
  (let [{:keys [name url email] :or {name "N/A" url "N/A" email "N/A" }} contact] 
    (->> ""
         (str "| \n")
         (str (str " | " email))
         (str (str " | " url))
         (str (str " | " name))
         (str "| Name | Url | E-mail | \n")
         (str "#####Contact info\n"))))

(defn license-str [{:keys [:license]}]
  ;;Converts the license part of the info to a markdown table. Needs fixing
  (let [{:keys [name url] :or {url "N/A"}} license]
    (->> ""
         (str "| \n")
         (str (str " | " url))
         (str (str " | " name))
         (str "| Name | Url | \n")
         (str "#####License info\n"))))

(defn json-to-markdown [info string]
  ;;Converts specific parts. If the part doesn't excist the regex fails and does nothing
  (-> string
    (.replaceAll "[ {]*:title \\\"(.*?)\\\"[,}]" "###$1\n")
    (.replaceAll "[ {]*:version \\\"(.*?)\\\"[,}]" "Version: $1\n\n")
    (.replaceAll "[ {]*:description \\\"(.*?)\\\"[,}]" "$1\n\n")
    (.replaceAll "[ {]*:termsOfService \\\"(.*?)\\\"[,}]" "$1\n\n")
    (.replaceAll "[ {]*:contact [{](.*?)[}][,}]" (contact-str info))
    (.replaceAll "[ {]*:license [{](.*?)[}][,}]" (license-str info))))

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

(defn update-comp [component {:keys [produces consumes]}]
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

(defn create-ops [model models wid parent _id methods]
  
  (map
   (fn [[method {parameters :parameters response :responses :as data}]]
     (let [k (keys response)
           k (first k)]
       (clojure.pprint/pprint (get-in response [k])))
     (-> (api/map->Component {:name (name method) 
                              :description (generate-operation-description data models) 
                              :rootWorkspace (str wid) 
                              :model _id 
                              :parent (:_id parent) 
                              :method method
                              :typeId (api/type-id-by-name model "Operation")}) 
         (api/create client) 
         (assoc :return-model (keyword nil)
                :input-models (set (map keyword (keep :type parameters))))))
   methods))

(defn create-methods [model models wid _id path spec {:keys [component]} methods] 
  ;; Used to create all methods for the resources and links them with the parent
  (update-comp component spec)
  (create-ops model models wid component _id methods))


(defn save-models [models client]
  (map-vals #(let [schema (:schema %)]
               (assoc (api/create (dissoc % :schema) client) :schema schema)) models))

(defn find-nested-model-deps [model]
  (letfn [(keys-in [m] (if (map? m)
                         (vec
                          (mapcat (fn [[k v]]
                                    (let [sub (keys-in v)
                                          nested (map #(into [k] %) (filter (comp not empty?) sub))]
                                      (if (seq nested)
                                        nested
                                        [[k]])))
                                  m))
                         []))]
    (keep #(let [r (last %)]
             (when (= :$ref r)
               (get-in model %)))
          (keys-in model))))

(defn interdependent-model-refs [client models]
  (mapcat
   (fn [model]
     (let [rrr (find-nested-model-deps (:schema model))]
       (keep
        (fn [model-key]
          (if-let [m ((keyword model-key) models)]
            (-> (api/map->Reference {:rootWorkspace (:rootWorkspace model)
                                     :source (str (:_id model))
                                     :target (str (:_id m))
                                     :type 3})
                (api/create client))))
        rrr)))
   (vals models)))

(defn create-refs [operations models]
  (concat (mapcat
           (fn [{input-models :input-models return-model :return-model id :_id :as comp}]
             (clojure.pprint/pprint return-model)
             (let [input-refs
                   (keep (fn [k]
                           (if-let [m (k models)]
                             (-> (api/map->Reference {:rootWorkspace (:rootWorkspace comp)
                                                      :source (str id)
                                                      :target (str (:_id m))
                                                      :type 1})
                                 (api/create client))))
                         input-models)]
               (if (and return-model
                        (return-model models))
                 (conj input-refs
                       (-> (api/map->Reference {:rootWorkspace (:rootWorkspace comp)
                                                :source (str id)
                                                :target (str (:_id (return-model models)))
                                                :type 0})
                           (api/create client)))
                 input-refs)))
           operations)
          ;(interdependent-model-refs client models)
          ))

(defn create-defs [{:keys [paths] :as spec} workspace]
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-models model wid _id paths spec)
        (save-models client))))

(defn create-resource [{:keys [paths definitions] :as spec} defs workspace]
  ;;Create a resource. Does so by setting first path resource then adding the operations to it. Requires a full swagger file as input and the workspace it is being created in
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (doall
     (map (fn [[path methods]]
            (let [parent (doall {:resource path
                                 :component (-> (api/->Component path description (str wid) _id (api/type-id-by-name model "Resource") nil)
                                                (api/create client))})
                  operations (create-methods model defs wid _id path spec parent methods)]
              (create-refs operations defs)))
          paths))
    (find-or-create-fields client model)
    ))


(defn get-info [spec]
  ;Converts the info from a Swagger 2 map to a string - This method needs to be redone
  (let [workspace (create-workspace nil client spec)
        defs (create-defs spec workspace)]
    (create-resource spec defs workspace)))


(defn get-data [spec]
  ;;Extracts data from a given Swagger file into an emtpy object
   (->> {}
       (parse-info spec)
       (parse-paths spec)
       (parse-definitions spec)
       (parse-produces spec)
       (parse-consumes spec)
       (get-info))
 (println 'done))


(comment 

  (loop [parent this
         acc {}]
    (let [processed-node (...)
          new-parent (...)]
      (if ..
        processed-node
        (recur new-parent proccessed-node))))

  (cond
   (get-in m [:response :type]) (handle-type ...)
   (get-in m [:response :$schema] (handle-schema ...))
   :else (explode))



)



;; ISSUES
;; Parameters to operations get created wrongly
;; start off the ref details get wrong data as a result
;; Description details for methods missing
;; Question if doing methods as a sub thing of paths is the correct way.
;; Will create a new structure thats very different from the v.1
;; How efficent is extracting maps from maps all the time? 
;; Create 5 different maps instead of one perhaps?

