(ns hlisp.compiler
  (:require [cljs.reader :as reader]) 
  (:use
    [hlisp.util :only [zipfn
                       tee]]
    [hlisp.hexp :only [make-node-hexp
                       make-data-hexp
                       make-text-hexp]]))

(declare compile-form decompile-hexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Compiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-attrs-pairs [s]
  (map
    #(list (first %)
           (if (string? (second %)) (second %) ""))
    (filter #(symbol? (first %))
            (partition 2 (interleave s (concat (rest s) (list (last s))))))))

(defn read-attrs [s]
  (into {} (vec (map vec (read-attrs-pairs s)))))

(defn compile-form [[tagsym attrs & children :as s]]
  (let [tag         (str tagsym)
        make-attrs  (comp read-attrs first)]
    (cond
      (= \# (first tag))
      (make-text-hexp tag attrs)

      (= "val:str" tag)
      (make-data-hexp (apply str (mapv second children)))

      :else
      (make-node-hexp tag (make-attrs attrs) (map compile-form children)))))

(defn compile-forms [forms]
  (map compile-form forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Decompiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn decompile-hexp [{:keys [tag attrs children text] :as hexp}]
  (cond
    (= \# (first tag))
    (list (symbol tag) text)

    (string? tag)
    (concat (list (symbol tag) (list (mapcat (zipfn [symbol str]) attrs)))
            (map decompile-hexp children))))

(defn decompile-hexps [hexps]
  (map decompile-hexp hexps))

