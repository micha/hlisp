(ns hlisp.interp
  (:require [clojure.set])
  (:use [hlisp.reader :only [read-form]]))

;;;;;;;;;;;;;;;;;;;;;; Utility functions ;;;;;;;;;;;;;

(defn to-seq [x]
  (if (seq? x)
    x
    (list x)))

;;;;;;;;;;;;;;;;;;;;;; Special tags ;;;;;;;;;;;;;;;;;;

(def html-tags
  #{ "a" "abbr" "acronym" "address" "applet" "area" "article" "aside"
     "audio" "b" "base" "basefont" "bdi" "bdo" "big" "blockquote" "body" "br"
     "button" "canvas" "caption" "center" "cite" "code" "col" "colgroup"
     "command" "data" "datalist" "dd" "del" "details" "dfn" "dir" "div"
     "dl" "dt" "em" "embed" "eventsource" "fieldset" "figcaption" "figure"
     "font" "footer" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5"
     "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img" "input"
     "ins" "isindex" "kbd" "keygen" "label" "legend" "li" "link" "map"
     "mark" "menu" "meta" "meter" "nav" "noframes" "noscript" "object" "ol"
     "optgroup" "option" "output" "p" "param" "pre" "progress" "q" "rp"
     "rt" "ruby" "s" "samp" "script" "section" "select" "small" "source"
     "span" "strike" "strong" "style" "sub" "summary" "sup" "table" "tbody"
     "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "track" "tt"
     "u" "ul" "var" "video" "wbr" })

(def html-text-tags
  #{ "#text" "#comment" })

(def hlisp-boxed-tags
  #{ "val:str" "val:num" "val:seq" "val:vec" "val:set" "val:map" "val:true"
     "val:false" "val:nil" })

(def self-evaluating-tags
  (clojure.set/union html-tags html-text-tags hlisp-boxed-tags))

;;;;;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;;

(defn make-env
  ([parent]
   (atom {:parent parent :bindings (atom {})})))

(defn set-env! [env name value]
  (swap! (:bindings env) #(assoc %1 %2 %3) name value))

(defn get-env [env name]
  (when (env)
    (let [parent    (:parent env)
          bindings  (:bindings env)]
      (or (find @bindings name)
          (get-env parent name)))))

(def global-env (make-env nil))

;;;;;;;;;;;;;;;;;;;;;; Hexp ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Hexp [tag attrs children text params env proc data])

(defn make-hexp [tag]
  (Hexp. tag {} [] "" [] {} nil nil))

(defn make-node-hexp [tag attrs children]
  (assoc (make-hexp tag) :attrs attrs :children children))

(defn make-text-hexp [tag text]
  (assoc (make-hexp tag) :text (str text)))

(defn make-data-hexp [tag data]
  (assoc (make-hexp tag) :data data))

(defn make-proc-hexp [params env proc]
  (assoc (make-hexp tag) :params params :env env :proc proc))

;;;;;;;;;;;;;;;;;;;;;; Compiler ;;;;;;;;;;;;;;;;;;;;;;

(defn read-attrs-pairs [s]
  (map
    #(list (first %)
           (if (string? (second %)) (second %) ""))
    (filter #(symbol? (first %))
            (partition 2 (interleave s (concat (rest s) (list (last s))))))))

(defn read-attrs [s]
  (into {} (vec (map vec (read-attrs-pairs s)))))

;;;;;;;;;;;;;;;;;;;;;; Analyzer ;;;;;;;;;;;;;;;;;;;;;;

(defn text-hexp? [hexp]
  (= \# (get (:tag hexp) 0)))

(defn self-evaluating-hexp? [hexp]
  (or (contains? self-evaluating-tags (:tag hexp))
      (text-hexp? hexp)))

(defn has-tag? [tag hexp]
  (= tag (:tag hexp)))

(def quoted-hexp? (partial has-tag? "quote"))
(def def-hexp?    (partial has-tag? "def"))
(def defn-hexp?   (partial has-tag? "defn"))
(def fn-hexp?     (partial has-tag? "fn"))

(defn nodes [hexps]
  (remove text-hexp? hexps))

(defn analyze-self-evaluating [hexp]
  (when (self-evaluating-hexp? hexp)
    (let [children (mapv analyze (:children hexp))]
      (fn [env]
        (assoc hexp :children (mapv #(% env) children))))))

(defn analyze-quoted [hexp]
  (when (quoted-hexp? hexp)
    (fn [env]
      (first (nodes (:children hexp))))))

(defn analyze-def [hexp]
  (when (def-hexp? hexp)
    (let [children  (nodes (:children hexp))
          name      (:tag (first children))  
          proc      (analyze (second children))]
      (fn [env]
        (let [val (proc env)]
          (set-env! env name val))))))

(defn analyze-defn [hexp])

(defn analyze-fn [hexp]
  (when (fn-hexp? hexp) ))

(defprotocol IHexp
  (analyze [hexp]))

(extend-protocol IHexp
  Hexp
  (analyze [hexp]
    (or
      (analyze-self-evaluating  hexp)
      (analyze-quoted           hexp)
      (throw (js/Error. (str hexp " is not a valid expression"))))))



