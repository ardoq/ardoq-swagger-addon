(ns ardoq.swagger.model-utils)

(defn to-component-type-map
  "Takes the root of a model and flattens it returning a typeId->type-map map"
  [model]
  (when model
    (letfn [(flatten-model [nodes res]
              (if (empty? nodes)
                res
                (let [{id :id children :children :as node} (first nodes)
                      r (assoc res (keyword id)
                          (update-in node [:children]
                            #(vec (map (comp name first) %))))
                      updated-children (map (fn [[_ i]] (assoc i :parent id))
                                         children)]
                  (flatten-model (concat (rest nodes) updated-children) r))))]
      (flatten-model (map (fn [[_ i]] (assoc i :parent nil)) (:root model)) {}))))

(defn type-id-by-name [model type-name]
  (some->> (to-component-type-map model)
    (vals)
    (filter #(= type-name (:name %)))
    (first)
    (:id)))

(defn type-ids-by-name [model]
  (some->>
    (to-component-type-map model )
    (map (fn [[k v]] [(:name v) k]))
    (into {})))

(defn reference-type-id-from-name [model reference-type-name]
  (:id (first (filter #(= (:name %) reference-type-name) (vals (:referenceTypes model))))))