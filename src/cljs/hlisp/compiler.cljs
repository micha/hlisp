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

      (= "val:num" tag)
      (make-data-hexp (cljs.reader/read-string (second (first children))))

      (= "val:nil" tag)
      (make-data-hexp nil)

      (= "val:true" tag)
      (make-data-hexp true)

      (= "val:false" tag)
      (make-data-hexp false)

      :else
      (make-node-hexp tag (make-attrs attrs) (map compile-form children)))))

(defn compile-forms [forms]
  (map compile-form forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Decompiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn decompile-hexp [{:keys [tag attrs children text ids] :as hexp}]
  (cond
    (= \# (first tag))
    (list (symbol tag) text)

    (string? tag)
    (let [attrmap (if (seq ids)
                    (assoc attrs :hl (clojure.string/join " " ids))
                    attrs)]
      (concat (list (symbol tag) (list (mapcat (zipfn [symbol str]) attrmap)))
              (remove nil? (map decompile-hexp children))))))

(defn decompile-hexps [hexps]
  (remove nil? (map decompile-hexp hexps)))

(defn dc [{:keys [tag attrs children text ids data] :as hexp}]
  (cond
    (= \# (first tag))
    (list (symbol tag) text)

    (= :data tag)
    (list tag data)

    (seq children)
    (concat (list (symbol tag))
            (remove nil? (map dc children)))

    :else
    (symbol tag)
    ))

(def dcs (partial map dc))
