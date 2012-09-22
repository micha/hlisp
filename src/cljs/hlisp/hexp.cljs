(ns hlisp.hexp)

(defn make-hexp [tag]
  {:tag         tag
   :ids         [(gensym)]
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

(defn make-proc-hexp [tag attr-params params env proc]
  (assoc (make-hexp tag)
         :attr-params attr-params
         :params      params
         :env         env
         :proc        proc))

(defn make-data-hexp [data]
  (assoc (make-hexp :data) :data data))

(defrecord Foo [a b]) 

(extend-type Foo
  IPrintable
  (-pr-seq [o] (list "Foo:<" (:a o) "," (:b o) ">")))


