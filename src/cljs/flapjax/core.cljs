(ns flapjax.core
  (:require [jayq.core :as jq]
            [jayq.util :as ju]))

(def *clicksE* (js/clicksE (.-body js/document)))

(defn filter-id [id]
  (fn [v]
    (< 0 (-> (jq/$ (.-target v))
           (.parentsUntil "body")
           (.andSelf)
           (.filter (str "[hl~='" id "']"))
           (.size)))))

