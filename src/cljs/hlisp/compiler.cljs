(ns hlisp.compiler
  (:use
    [hlisp.util :only [zipfn]]
    [hlisp.hexp :only [make-hexp
                       make-node-hexp
                       make-seq-hexp
                       make-text-hexp
                       make-prim-hexp
                       make-proc-hexp]]))

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

(defn compile-text-hexp [s]
  (let [[tagsym text] s
        tag (str tagsym)]
    (when (= \# (first tag))
      (make-text-hexp tag text))))

(defn compile-node-hexp [s]
  (let [[tagsym [[& attrlist]] & children] s
        tag (str tagsym)]
    (when (not= \# (first tag))
      (make-node-hexp tag
                      (read-attrs attrlist)
                      (map compile-form children)))))

(defn compile-form [s]
  (or (compile-text-hexp s)
      (compile-node-hexp s)
      (assert false (str "compile: " s " is not a valid expression"))))

(defn compile-forms [forms]
  (map compile-form forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Decompiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn decompile-text-hexp [hexp]
  (let [{:keys [tag text]} hexp]
    (when (= \# (first tag))
      (list (symbol tag) text))))

(defn decompile-node-hexp [hexp]
  (let [{:keys [tag attrs children]} hexp]
    (when (string? tag)
      (concat
        (list (symbol tag)
              (list (mapcat (zipfn [symbol str]) attrs)))
        (map decompile-hexp children)))))

(defn decompile-hexp [hexp]
  (or (decompile-text-hexp hexp)
      (decompile-node-hexp hexp)))

(defn decompile-hexps [hexps]
  (map decompile-hexp hexps))

