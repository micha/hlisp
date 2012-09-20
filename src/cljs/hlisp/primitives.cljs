(ns hlisp.primitives
  (:use
    [hlisp.util :only [tee]]
    [hlisp.hexp :only [make-hexp
                       make-data-hexp
                       make-node-hexp
                       make-text-hexp
                       make-prim-hexp
                       make-proc-hexp]]))

(def prims
  [

   "foop"
   (fn [{:syms [hey] :as attr} args]
     (assoc
       (make-hexp "div")
       :children (vec args)
       :attrs (into {'asdf hey} attr)))

   "str"
   (fn [_ args]
     (make-data-hexp (apply str (map :data args))))

   ])
