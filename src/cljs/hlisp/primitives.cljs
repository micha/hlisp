(ns hlisp.primitives
  (:require-macros [hlisp.macros :as macros])
  (:require [jayq.core    :as jq]
            [jayq.util    :as ju]
            [flapjax.core :as fj])
  (:use
    [hlisp.util         :only [tee]]
    [hlisp.interpreter  :only [apply*
                               truthy-hexp?]]
    [hlisp.compiler     :only [dc
                               dcs]]
    [hlisp.hexp         :only [make-hexp
                               make-data-hexp
                               make-node-hexp
                               make-text-hexp
                               make-list-hexp
                               make-prim-hexp
                               make-proc-hexp]]))

(defn dom-assoc! [_ [elem attr-name attr-val]]
  (let [id (peek (:ids elem))
        k  (:data attr-name)
        v  (:data attr-val)]
    (-> (jq/$ (str "[hl~='" id "']")) (.attr k v))
    elem))

(defn dom-dissoc! [_ [elem attr-name]]
  (let [id (peek (:ids elem))
        k  (:data attr-name)]
    (-> (jq/$ (str "[hl~='" id "']")) (.removeAttr k))
    elem))

(def prims
  [


   "dom-add-class!"
   (fn [_ [elem class-name]]
     (let [id (peek (:ids elem))
           c  (:data class-name)]
       (-> (jq/$ (str "[hl~='" id "']")) (.addClass c))
       elem))

   "dom-remove-class!"
   (fn [_ [elem class-name]]
     (let [id (peek (:ids elem))
           c  (:data class-name)]
       (-> (jq/$ (str "[hl~='" id "']")) (.removeClass c))
       elem))

   "dom-toggle!"
   (fn [_ [elem val]]
     (let [id (peek (:ids elem))]
       (-> (jq/$ (str "[hl~='" id "']")) (.toggle (truthy-hexp? val)))))

   "="
   (fn [_ args]
     (make-data-hexp (apply = args)))

   "not="
   (fn [_ args]
     (make-data-hexp (apply not= args)))

   "concat"
   (fn [_ args]
     (let [children (vec (mapcat :children args))]
       (assoc (first args) :children children)))

   "call"
   (fn [_ [f & args]]
     (if-not (seq args) f (apply* f {} args)))

   "gensym"
   (fn [_ _]
     (make-data-hexp (str (gensym))))

   "log"
   (fn [_ args]
     (js/console.log (apply str (map :data args)))
     (make-data-hexp nil))

   "log-obj"
   (fn [_ args]
     (tee dcs "[log-obj]" args)
     (make-data-hexp nil))

   "str"
   (fn [_ args]
     (make-data-hexp (apply str (map :data args))))

   "clone"
   (fn [_ [{:keys [ids] :as hexp}]]
     (assoc hexp :ids (conj ids (gensym))))

   "clicksE"
   (fn [_ [{:keys [ids] :as hexp}]]
     (make-data-hexp
       (js/filterE
         (js/clicksE (.-body js/document))
         (fj/filter-id (peek ids)))))

   "oneE"
   (fn [_ [e]]
     (make-data-hexp
       (js/oneE (:data e))))

   "mapE"
   (fn [_ [f e]]
     (make-data-hexp
       (js/mapE
         (fn [v] (apply* f {} [(make-data-hexp v)]))
         (:data e))))

   "filterE"
   (fn [_ [pred src]]
     (make-data-hexp
       (js/filterE
         (:data src)
         (fn [v] (truthy-hexp? (apply* pred {} [(make-data-hexp v)]))))))

   "receiverE"
   (fn [_ _]
     (make-data-hexp
       (js/receiverE)))

   "sendE"
   (fn [_ [rcv e]]
     (.sendEvent (:data rcv) (:data e)) 
     (make-data-hexp nil))

   ])
