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

()

(defn create-ops [client model models wid parent _id methods]
  (map
   (fn [[method {parameters :parameters response :responses :as data}]]
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
                  :input-models parameters))))
   methods))

(defn create-methods [client model models wid _id path spec {:keys [component]} methods] 
  ;; Used to create all methods for the resources and links them with the parent
  (update-comp client component spec)
  (create-ops client model models wid component _id methods))


(defn save-models [models client]
  (map-vals #(let [schema (:schema %)]
               (assoc (api/create (dissoc % :schema) client) :schema schema)) models))

(defn find-nested-model-deps [model]
  (map (fn [v]
         (last (.split v "/")))
   (keep :$ref (tree-seq #(or (map? %) (vector? %)) identity model))))

(defn interdependent-model-refs [client models]
  (mapcat
   (fn [model]
     (let [rrr (find-nested-model-deps (:schema model))]
       (doall (keep
               (fn [model-key]
                 (if-let [m ((keyword model-key) models)]
                   (-> (api/map->Reference {:rootWorkspace (:rootWorkspace model)
                                            :source (str (:_id model))
                                            :target (str (:_id m))
                                            :type 3})
                       (api/create client))))
               rrr))))
   (vals models)))

(defn create-refs [client operations models]
  (concat (mapcat
           (fn [{input-models :input-models return-models :return-model id :_id :as comp}]
             (let [input-models (set input-models)
                   return-models (set return-models)]
               (let [input-refs 
                     (doall (keep (fn [k]
                                    (let [k (cond 
                                             ;NOTE This only accepts the first link if there are multiple in nested maps. Should rework a bit
                                             (seq (find-nested-model-deps (:schema k))) (last (.split (first (find-nested-model-deps (:schema k))) "/"))
                                             (get-in k [:type]) (last (.split (get-in k [:type]) "/"))
                                             :else "nil")
                                          k (keyword k)]                   
                                      (if-let [m (k models)]
                                        (-> (api/map->Reference  {:rootWorkspace (:rootWorkspace comp)
                                                                  :source (str id)
                                                                  :target (str(:_id m))
                                                                  :type 1})
                                            (api/create client)))))
                                  input-models))])
               (doall (keep (fn [k]
                              (let [k
                                    (cond 
                                     (get-in k [:$ref]) (get-in k [:$ref])
                                     :else "nil")
                                    k (keyword (last (.split k "/")))]
                                (if-let [m (k models)]
                                  (-> (api/map->Reference {:rootWorkspace (:rootWorkspace comp)
                                                           :source (str id)
                                                           :target (str (:_id m))
                                                           :type 0})
                                      (api/create client)))))
                            return-models))))
           operations)))

(defn create-defs [client {:keys [paths] :as spec} workspace]
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (-> (create-models model wid _id paths spec)
        (save-models client))))

(defn create-resource [client {:keys [paths definitions] :as spec} defs workspace]
  ;;Create a resource. Does so by setting first path resource then adding the operations to it. Requires a full swagger file as input and the workspace it is being created in
  (let [model (find-or-create-model client)
        {:keys [_id description]} model
        wid (:_id workspace)]
    (doall
     (map (fn [[path methods]]
            (let [parent (doall {:resource path
                                 :component (-> (api/->Component path description (str wid) _id (api/type-id-by-name model "Resource") nil)
                                                (api/create client))})
                  operations (create-methods client model defs wid _id path spec parent methods)]
              (create-refs client operations defs)))
          paths))
    (interdependent-model-refs client defs)
    (find-or-create-fields client model)
    ))


(defn get-info [client spec]
  ;Converts the info from a Swagger 2 map to a string - This method needs to be redone
  (let [workspace (create-workspace nil client spec)
        defs (create-defs client spec workspace)]
    (create-resource client spec defs workspace)))


(defn get-data [client spec]
  ;;Extracts data from a given Swagger file into an emtpy object
   (->> {}
       (parse-info spec)
       (parse-paths spec)
       (parse-definitions spec)
       (parse-produces spec)
       (parse-consumes spec)
       (get-info client)))

(defn get-resource-listing [url headers]
  (let [{:keys [status body] :as resp} @(http/get (str (io/as-url url)) {:headers headers :insecure? true})]
    (println "\nResponse from " url "\n")
    (if (= 200 status)
      (parse-string body true)
      (throw (IllegalArgumentException. (str "Unexpected response " status " from " url))))))

(defn import-swagger2 [client base-url name headers]
  (println "Importing swagger doc from " base-url ". Custom headers" headers)
  (let [spec (get-resource-listing base-url headers)]
    (clojure.pprint/pprint spec)
    (get-info client spec))
  (println "Done importing swagger doc from " base-url "."))


;; ISSUES
;; Create a proper import method
;; Check if you can have other than just #/def/whatever and whatever

