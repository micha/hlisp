(ns hlisp.reader
  (:require [clojure.browser.repl :as repl]
            [cljs.reader :as reader]))

(defn third [s]
  (first (rest (rest s))))

(defn valid-tag? [tag]
  (and
    (string? tag)
    (re-matches #"^#?[a-zA-Z0-9_:-]+$" tag)))

(defn mkexp
  ([]
   "Empty expression."
   {:type "" :attr {} :chld [] :text "" :aparams {} :cparams [] :env {} :proc nil})

  ([type]
   "Minimal expression, tag only."
   (if (valid-tag? type)
     (assoc (mkexp) :type (str type))
     (throw (js/Error. (str type " is not a valid tag")))))

  ([type & more]
   (reduce
     (fn [xs x]
       (let [y (js->clj x)]
         (assoc
           xs 
           (cond 
             (string? y) :text
             (map?    y) :attr
             (vector? y) :chld
             :else (throw (js/Error. (str y " is not a string, vector, or map"))))
           y)))
     (mkexp type)
     more)))

(defn read-attrs-pairs [s]
  (map
    #(list (first %)
           (if (string? (second %)) (second %) (str (first %))))
    (filter #(symbol? (first %))
            (partition 2 (interleave s (concat (rest s) (list (last s))))))))

(defn read-attrs [s]
  (into {} (vec (map vec (read-attrs-pairs s)))))

(defn read-list [s]
  (cond
    (< (count s) 1) (throw (js/Error. (str "empty list read error")))
    (< (count s) 2) (mkexp (str (first s)))
    :else 
    (if (and (list? (second s)) (list? (first (second s))))
      (mkexp (str (first s)) 
             (read-attrs (first (second s)))
             (if (string? (third s))
               [(mkexp "#text" (third s))] 
               (vec (map read-exp (rest (rest s)))))) 
      (read-list (concat (list (first s) '(())) (rest s))))))

(defn boxed? [s]
  (and (list? s) (= (first s) 'quote) (string? (second s))))

(defn read-exp [s]
  (cond
    (symbol? s) (mkexp (str s))
    (boxed?  s) (read-list (list 'val (second s)))
    (list?   s) (read-list s)
    (vector? s) (read-list (cons 'list s))
    :else       (throw (js/Error. (str s " read error")))))

(defn read [s]
  (map read-exp (reader/read-string (str \( s \newline \)))))

(defn doit [s]
  (doall (read s)))

