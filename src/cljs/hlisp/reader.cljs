(ns hlisp.reader
  (:require [cljs.reader :as reader]))

(def empty-attrs '(()))

(defn valid-list-expr? [expr]
  (and (seq? expr) (symbol? (first expr))))

(defn valid-attrlist? [attrs]
  (and (seq? attrs) (seq? (first attrs))))

(defn text-node [txt]
  (list (symbol "#text") (str txt)))

(defn parse-symbol [expr]
  (when (symbol? expr)
    (list expr empty-attrs)))

(defn parse-text-node [expr]
  (when (string? expr)
    (text-node expr)))

(defn parse-seqable-literal [pred valtag]
  (fn [expr]
    (when (pred expr)
      (concat (list valtag empty-attrs)
              (map normal-form expr)))))

(defn parse-atomic-literal [pred valtag]
  (fn [expr]
    (when (seq? expr)
      (let [[tag thing] expr]
        (when (and (= 2 (count expr))
                   (= 'quote tag)
                   (pred thing))
          (list valtag empty-attrs (text-node (str thing))))))))

(defn parse-map-literal [expr]
  (when-let [m ((parse-seqable-literal map? 'val:map) expr)]
    (concat (take 2 m) (mapcat (drop 2 m)))))

(defn parse-set-literal [expr]
  ((parse-seqable-literal set? 'val:set) expr))

(defn parse-vector-literal [expr]
  ((parse-seqable-literal vector? 'val:vec) expr))

(defn parse-string-literal [expr]
  ((parse-atomic-literal string? 'val:str) expr))

(defn parse-number-literal [expr]
  ((parse-atomic-literal number? 'val:num) expr))

(defn parse-list [expr]
  (when (valid-list-expr? expr)
    (let [[tag attrs & children] expr]
      (if (valid-attrlist? attrs)
        (concat (list tag attrs)
                (map normal-form children))
        (parse-list
          (remove nil? (concat (list tag empty-attrs attrs) children)))))))

(defn normal-form [expr]
  (or
    (parse-symbol         expr)
    (parse-text-node      expr)
    (parse-map-literal    expr)
    (parse-set-literal    expr)
    (parse-vector-literal expr)
    (parse-string-literal expr)
    (parse-number-literal expr)
    (parse-list           expr)
    (throw (js/Error. (str expr " isn't a valid expression")))))

