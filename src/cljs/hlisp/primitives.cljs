(ns hlisp.primitives
  (:use
    [hlisp.hexp :only [make-hexp
                       make-node-hexp
                       make-seq-hexp
                       make-text-hexp
                       make-prim-hexp
                       make-proc-hexp]]))

(def prims
  ["foop"
   (fn [{:syms [hey] :as attr} args]
     (assoc
       (make-hexp "div")
       :children (vec args)
       :attrs (into {'asdf hey} attr)))] 

  )
