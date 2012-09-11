(ns hlisp.reader
  (:require [cljs.reader :as reader]))

(def empty-attrs '(()))

(defn text-node [txt]
  (list (symbol "#text") (str txt)))

(defn parse-symbol [expr]
  (when (symbol? expr)
    (list expr empty-attrs)))

(defn parse-text-node [expr]
  (when (string? expr)
    (text-node expr)))

(defn parse-map-literal [expr]
  (when (map? expr)
    (concat (list 'val:map empty-attrs)
            (map normal-form (apply concat (vec expr))))))

(defn parse-set-literal [expr]
  (when (set? expr)
    (concat (list 'val:set empty-attrs)
            (map normal-form (vec expr)))))

(defn parse-literal [pred valtag]
  (fn [expr]
    (when-let [[tag thing] expr]
      (when (and (= 2 (count expr))
                 (= 'quote tag)
                 (pred thing))
        (list valtag empty-attrs (text-node (str thing)))))))

(defn parse-string-literal [expr]
  ((parse-literal string? 'val:str) expr))

(defn parse-number-literal [expr]
  ((parse-literal number? 'val:num) expr))

(defn parse-list [expr]
  (when (and (seq? expr)
             (symbol? (first expr))) 
    (let [[tag attrs & children] expr]
      (if (and (seq? attrs)
               (seq? (first attrs)))
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
    (parse-string-literal expr)
    (parse-number-literal expr)
    (parse-list           expr)
    (throw (js/Error. (str expr " isn't a valid expression")))))

