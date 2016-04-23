(ns mmbot.util.core
    (:require
      [clj-http.client :as http]
      [cheshire.core :refer :all :as json]))

(def CODE-UNION "ABC123XYZ")
(def CODE-EMPTY "")

(defn parse-responce
      [responce]
      (-> responce
          :body
          (json/parse-string true)))

(defn get-message
      [all-upd]
      (-> all-upd first :message))

(defn par-query-string
  "some util function"
  [p1-val p1-text]
  (cond
    (= p1-val CODE-EMPTY)
    CODE-EMPTY
    (= p1-val CODE-UNION)
    p1-text
    :else
    (str p1-text "=" p1-val)))