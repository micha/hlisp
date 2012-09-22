(ns hlisp.primitives
  (:require [jayq.core :as jq]
            [jayq.util :as ju])
  (:use
    [hlisp.util         :only [tee]]
    [hlisp.interpreter  :only [apply*]]
    [hlisp.hexp         :only [make-hexp
                               make-data-hexp
                               make-node-hexp
                               make-text-hexp
                               make-prim-hexp
                               make-proc-hexp]]))

(defn filter-e [id]
  (fn [v]
    (< 0 (-> (jq/$ (.-target v))
           (.parentsUntil "body")
           (.andSelf)
           (.filter (str "[hl~='" id "']"))
           (.size)))))

(def prims
  [

   "concat"
   (fn [_ args]
     (let [children (vec (mapcat :children args))]
       (assoc (first args) :children children)))

   "call"
   (fn [_ args]
     (apply* (first args) {} (rest args)))

   "log"
   (fn [_ args]
     (js/console.log (apply str (map :data args))))

   "str"
   (fn [_ args]
     (make-data-hexp (apply str (map :data args))))

   "clone"
   (fn [_ [{:keys [ids] :as hexp}]]
     (assoc hexp :ids (conj ids (gensym))))

   "clicksE"
   (fn [_ [{:keys [ids] :as hexp}]]
     (js/mapE
       (fn [v] (js/console.log v))
       (js/filterE
         (js/clicksE (.-body js/document))
         (filter-e (str (peek ids)))))
     hexp)

   ])
