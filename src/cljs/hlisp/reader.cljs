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

(defn normalize-exp [s]
  {:pre [(valid-exp? s)]}
  (cond
    (symbol?     s)  (list s empty-attrs)
    (number?     s)  (list 'val:num empty-attrs (text-node s))
    (string?     s)  (list 'val:str empty-attrs (text-node s)) 
    (vector?     s)  (cons 'val:vec s)
    (map?        s)  (cons 'val:map s) 
    (set?        s)  (cons 'val:set s)
    (text-node?  s)  (text-node (first s))
    (not (attrs? s)) (normalize-exp (concat (list (first s) empty-attrs) (rest s))) 
    (branch?     s)  (concat (take 2 s) (map normalize-exp (drop 2 s)))
    (leaf?       s)  s
    :else
    (throw (js/Error. (str s " read error")))))

(defn expr [s]
  (build-exp (normalize-exp s)))
