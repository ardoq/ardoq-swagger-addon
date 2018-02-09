(ns ardoq.swagger.map-common
  (:require [ardoq.swagger.socket :refer [socket-send]]
            [clojure.string :as s]))


(defn transform-objects [data transform-object-fn param-spec parent-key spec-type]
  (reduce
    (fn [data object-spec]
      (if (map-entry? object-spec)
        (apply transform-object-fn [(key object-spec) parent-key data (val object-spec) spec-type])
        (apply transform-object-fn [(or (:name object-spec) (name spec-type)) parent-key data object-spec spec-type])))
    data
    param-spec))
