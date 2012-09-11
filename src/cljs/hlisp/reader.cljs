(ns hlisp.reader
  (:require [cljs.reader :as reader]))

(defn elem-tag? [tag]
  (not= \# (get (str tag) 0)))

(defn branch? [s]
  (elem-tag? (first s)))

(defn leaf? [s]
  (not (branch? s)))

(defn attrs? [s]
  (let [[tag attr] s]
    (or
      (and
        (< 1 (count s))
        (symbol? tag)
        (seq? attr)
        (= 1 (count attr))
        (seq? (first attr)))
      (not (elem-tag? tag)))))

(defn text-node? [s]
  (and
    (= 1 (count s))
    (string? (first s))))

(defn valid-exp? [s]
  (or (or (string? s)
          (number? s)
          (vector? s)
          (map?    s)
          (set?    s)
          (symbol? s))
      (and (seq? s)
           (symbol? (first s)))
      (text-node? s)))

(defn valid-tag? [tag]
  (and
    (string? tag)
    (re-matches #"^#?[a-zA-Z0-9_:-]+$" tag)))

(defn read-attrs-pairs [s]
  (map
    #(list (first %)
           (if (string? (second %)) (second %) ""))
    (filter #(symbol? (first %))
            (partition 2 (interleave s (concat (rest s) (list (last s))))))))

(defn read-attrs [s]
  (into {} (vec (map vec (read-attrs-pairs s)))))

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
    (when (seqable? expr)
      (let [[tag thing] expr]
        (when (and (= 2 (count expr))
                   (= 'quote tag)
                   (pred thing))
          (list valtag empty-attrs (text-node (str thing))))))))

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

