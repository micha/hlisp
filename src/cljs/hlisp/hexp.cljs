(ns hlisp.hexp)

(defn make-hexp [tag]
  {:tag         tag
   :attrs       {}
   :children    []
   :text        ""
   :attr-params {}
   :params      []
   :env         {}
   :proc        nil
   :data        nil})

(defn make-node-hexp [tag attrs children]
  (assoc (make-hexp tag) :attrs attrs :children children))

(defn make-seq-hexp [items]
  (make-node-hexp "val:seq" {} (vec items)))

(defn make-text-hexp [tag text]
  (assoc (make-hexp tag) :text (str text)))

(defn make-prim-hexp [proc]
  (assoc (make-hexp :prim) :proc proc))

(defn make-proc-hexp [attr-params params env proc]
  (assoc (make-hexp :proc)
         :attr-params attr-params
         :params      params
         :env         env
         :proc        proc))

