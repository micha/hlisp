(ns hlisp.interp
  (:require [clojure.set])
  (:use [hlisp.reader :only [read-form]]))

(declare compile-form analyze analyze-body analyze-seq text-hexp? eval-all) 

(defn do! [& _])

(def funroll-body
  (partial reduce (fn [x y] (fn [& args] (apply x args) (apply y args)))))

(defn funroll-seq [procs]
  (fn [& args]
    (map #(apply % args) procs)))

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

(def *global-env* (atom {}))

(def bind-env into)

(def bind-env! (partial swap! *global-env* into))

(defn resolve-env [env name]
  (or
    (get env name)
    (get @*global-env* name)))

;;;;;;;;;;;;;;;;;;;;;; Hexp ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Hexp [tag attrs children text attr-params params env proc data])

(defn make-hexp [tag]
  (Hexp. tag {} [] "" {} [] {} nil nil))

(defn make-node-hexp [tag attrs children]
  (assoc (make-hexp tag) :attrs attrs :children children))

(defn make-text-hexp [tag text]
  (assoc (make-hexp tag) :text (str text)))

(defn make-data-hexp [tag data]
  (assoc (make-hexp tag) :data data))

(defn make-proc-hexp [attr-params params env proc]
  (assoc (make-hexp "proc")
         :attr-params attr-params
         :params      params
         :env         env
         :proc        proc))

;;;;;;;;;;;;;;;;;;;;;; Compiler ;;;;;;;;;;;;;;;;;;;;;;

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
  (or (compile-text-hexp        s)
      (compile-node-hexp        s)
      (throw (js/Error. (str "compile: " s " is not a valid expression")))))

(defn compile-forms [forms]
  (map compile-form forms))

;;;;;;;;;;;;;;;;;;;;;; Syntactic Analysis ;;;;;;;;;;;;

(defn elems [hexps]
  (remove text-hexp? hexps))

(defn text-hexp? [hexp]
  (= \# (get (:tag hexp) 0)))

(defn self-evaluating-hexp? [hexp]
  (or (contains? self-evaluating-tags (:tag hexp))
      (text-hexp? hexp)))

(defn has-tag? [tag hexp]
  (= tag (:tag hexp)))

(def quoted-hexp? (partial has-tag? "quote"))
(def def-hexp?    (partial has-tag? "def"))
(def fn-hexp?     (partial has-tag? "fn"))

(defn application-hexp? [hexp]
  (seq (elems (:children hexp))))

(defn variable-hexp? [hexp]
  (not (application-hexp? hexp)))

(defn analyze-self-evaluating [hexp]
  (when (self-evaluating-hexp? hexp)
    (let [children (mapv analyze (:children hexp))]
      (fn [env]
        (assoc hexp :children (mapv #(% env) children))))))

(defn analyze-quoted [hexp]
  (when (quoted-hexp? hexp)
    (fn [env]
      (first (elems (:children hexp))))))

(defn analyze-def [hexp]
  (when (def-hexp? hexp)
    (let [children  (elems (:children hexp))
          name      (:tag (first children))  
          proc      (analyze (second children))]
      (fn [env]
        (let [val (proc env)]
          (do! (bind-env! {name val})))))))

(defn make-child-params [m]
  (mapv :tag m))

(defn analyze-fn [hexp]
  (when (fn-hexp? hexp)
    (let [[c-params & body] (elems (:children hexp)) 
          child-params      (mapv :tag (:children c-params))
          attr-params       (:attrs c-params)
          proc              (if (seq body)
                              (analyze-body body)
                              (throw (js/Error. "empty body")))]
      (fn [env]
        (make-proc-hexp attr-params child-params env proc)))))

(defn analyze-variable [hexp]
  (when (variable-hexp? hexp)
    (let [name (:tag hexp)]
      (fn [env]
        (or (resolve-env env name)
            (throw (js/Error. (str "unbound variable " name))))))))

(defn apply-fn [hexp args attr-args]
  (let [proc        (:proc        hexp)
        params      (:params      hexp)
        attr-params (:attr-params hexp)
        env         (bind-env (:env hexp) (zipmap params args))]
    (proc env)))

(defn analyze-application [hexp]
  (when (application-hexp? hexp)
    (let [proc      (analyze (make-hexp (:tag hexp))) 
          args      (analyze-seq (elems (:children hexp))) 
          attr-args (:attrs hexp)]
      (fn [env]
        (let [func (proc env)]
          (condp = (:tag func)
            "proc"  (apply-fn func (args env) attr-args)
            "fproc" (apply-fn func (args env) attr-args)
            (update-in func [:children] into (args env))))))))

(defprotocol IHexp
  (analyze [hexp]))

(extend-protocol IHexp
  Hexp
  (analyze [hexp]
    (or
      (analyze-self-evaluating  hexp)
      (analyze-quoted           hexp)
      (analyze-def              hexp)
      (analyze-fn               hexp)
      (analyze-variable         hexp)
      (analyze-application      hexp)
      (js/console.log "unknown expr type:" hexp)
      (throw (js/Error. (str "analyze: " hexp " is not a valid expression"))))))

(def analyze-body   (comp funroll-body (partial map analyze)))
(def analyze-seq    (comp funroll-seq (partial map analyze)))

(def analyze-forms  (comp analyze-seq compile-forms))

;;;;;;;;;;;;;;;;;;;;;; Evaluation ;;;;;;;;;;;;;;;;;;;;

(defn eval-forms [& forms]
  (remove nil? ((analyze-forms (hlisp.reader/read-forms (first forms))) {})))

(defn eval-string [s]
  (remove nil? ((analyze-forms (hlisp.reader/read-string s)) {})))


