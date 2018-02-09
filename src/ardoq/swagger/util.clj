(ns ardoq.swagger.util
  (:require
            [clojure.data.json :as json]
            [superstring.core :as str]
            [clojure.java.io :as io]
            [cheshire.core :refer [generate-string parse-string]]
            [clostache.parser :as tpl]
            [yaml.core :as yaml]
            [ardoq.yaml.reader :as ardoq-yaml]
            [hiccup.core :refer [html]]
            [clj-http.client :as http]
            [hiccup.form :refer [form-to submit-button text-field label hidden-field]]
            [compojure.route :as route]))


(defn parse-swagger [spec-text]
  (if (str/starts-with? (str/trim spec-text) "{")
    (parse-string spec-text true)
    (ardoq-yaml/parse-string spec-text)))
