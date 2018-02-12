(ns ardoq.swagger.map-common
  (:require [ardoq.swagger.socket :refer [socket-send]]
            [clojure.string :as s]))


(defn wrap [f]
  (fn [& args]
    (let [spec-key (str (second args) "/" (name (first args)))]
      (try
        (prn (str "Mapping " spec-key "to Ardoq component"))
        (socket-send (str "Mapping " spec-key "to Ardoq component"))
        (apply f args)
        (catch Exception e
          (throw (ex-info "Mapping Exception" {:mapping-key spec-key})))))))

(defn transform-objects [data transform-object-fn param-spec parent-key spec-type]
  (reduce
    (fn [data object-spec]
      (if (map-entry? object-spec)
        (apply (wrap transform-object-fn) [(key object-spec) parent-key data (val object-spec) spec-type])
        (apply (wrap transform-object-fn) [(or (:name object-spec) (name spec-type)) parent-key data object-spec spec-type])))
    data
    param-spec))
