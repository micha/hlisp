(ns hlisp.reader
  (:require [cljs.reader :as reader]))

(defn branch? [s]
  (and
    (symbol? (first s))
    (not= \# (get (str (first s)) 0))))

(defn leaf? [s]
  (and
    (= 3 (count s))
    (symbol? (first s))
    (string? (nth s 2))))

(defn boxed? [s]
  (and
    (= 2 (count s))
    (= (first s) 'quote)
    (string? (second s))))

(defn attrs? [s]
  (and
    (< 1 (count s))
    (symbol? (first s))
    (seq? (second s))
    (= 1 (count (second s)))
    (seq? (first (second s)))))

(defn valid-exp? [s]
  (or (or (string? s)
          (vector? s)
          (symbol? s))
      (and (seq? s)
           (symbol? (first s)))))

(defn valid-tag? [tag]
  (and
    (string? tag)
    (re-matches #"^#?[a-zA-Z0-9_:-]+$" tag)))

(defn mkexp
  ([]
   "Empty expression."
   {:type "" :attr {} :chld [] :text "" :aparams {} :cparams [] :env {} :proc nil})

  ([type attrs cnodes]
   (let [exp (assoc (mkexp) :type type :attr attrs)]
     (cond
       (vector? cnodes) (assoc exp :chld cnodes)
       (string? cnodes) (assoc exp :text cnodes)
       :else
       (throw (js/Error. (str cnodes " is not a vector or a string")))))))

(defn read-attrs-pairs [s]
  (map
    #(list (first %)
           (if (string? (second %)) (second %) ""))
    (filter #(symbol? (first %))
            (partition 2 (interleave s (concat (rest s) (list (last s))))))))

(defn read-attrs [s]
  (into {} (vec (map vec (read-attrs-pairs s)))))

(defn build-exp [s]
  (let [type (str (first s)) 
        attr (read-attrs (first (second s)))
        chld (if (leaf? s) (nth s 2) (vec (map build-exp (drop 2 s))))]
    (mkexp type attr chld)))

(def empty-attrs '(()))

(defn normalize-exp [s]
  (if (not (valid-exp? s))
    (throw (js/Error. (str s " read error!"))))
  (cond
    (symbol?     s)  (list s empty-attrs)
    (string?     s)  (list (symbol "#text") empty-attrs s)
    (vector?     s)  (normalize-exp (cons 'list s)) 
    (boxed?      s)  (normalize-exp (cons 'val (rest s)))
    (not (attrs? s)) (normalize-exp (concat (list (first s) empty-attrs) (rest s))) 
    (branch?     s)  (concat (take 2 s) (map normalize-exp (drop 2 s)))
    (leaf?       s)  s
    :else
    (throw (js/Error. (str s " read error")))))

