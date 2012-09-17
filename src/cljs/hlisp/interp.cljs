(ns hlisp.interp
  (:require [clojure.set])
  (:use [hlisp.reader :only [read-form]]))

(declare prims compile-form analyze analyze-body analyze-seq text-hexp? eval-all) 

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
(def bind-env     into)
(def bind-global! (partial swap! *global-env* into))

(defn resolve-env [env name]
  (or (get env name) (get @*global-env* name)))

;;;;;;;;;;;;;;;;;;;;;; Hexp ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Hexp [tag attrs children text attr-params params env proc data])

(defn make-hexp [tag]
  (Hexp. tag {} [] "" {} [] {} nil nil))

(defn make-node-hexp [tag attrs children]
  (assoc (make-hexp tag) :attrs attrs :children children))

(defn make-seq-hexp [items]
  (make-node-hexp "val:seq" {} (vec items)))

(defn make-text-hexp [tag text]
  (assoc (make-hexp tag) :text (str text)))

(defn make-prim-hexp [proc]
  (assoc (make-hexp :prim) :proc proc))

(defn make-proc-hexp [attr-params params env proc]
  (assoc (make-hexp :proc)
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
      (assert false (str "compile: " s " is not a valid expression"))))

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
          (bind-global! {name val})
          nil)))))

(defn parse-bindings [params args]
  (let [k (first params)
        v (first args)]
    (cond
      (= "&" k)
      (parse-bindings (rest params) (list (make-seq-hexp args)))
      (and k v)
      (into {k v} (parse-bindings (rest params) (rest args)))
      (or k v)
      (assert false "arity mismatch"))))

(defn analyze-fn [hexp]
  (when (fn-hexp? hexp)
    (let [[c-params & body] (elems (:children hexp)) 
          params            (mapv :tag (:children c-params))
          attr-params       (:attrs c-params)
          proc              (analyze-body body)]
      (assert (seq body) "empty body")
      (fn [env]
        (make-proc-hexp attr-params params env proc)))))

(defn apply-fn [hexp args attr-args]
  (let [proc        (:proc        hexp)
        params      (:params      hexp)
        attr-params (:attr-params hexp)
        env         (bind-env (:env hexp) (parse-bindings params args))]
    (proc env)))

(defn make-cljs-args [args attr-args]
  (into (vec args) (mapcat identity (vec attr-args))))

(defn apply-prim [hexp args attr-args]
  ((:proc hexp) attr-args args))

(defn analyze-node [hexp]
  (let [name      (:tag hexp) 
        args      (analyze-seq (elems (:children hexp))) 
        attr-args (:attrs hexp)]
    (fn [env]
      (let [proc (resolve-env env name)
            argv (args env)]
        (assert proc (str "eval: unbound variable " name))
        (cond
          (and (seq argv) (= :proc (:tag proc))) 
          (apply-fn proc (args env) attr-args)
          (and (seq argv) (= :prim (:tag proc))) 
          (apply-prim proc (args env) attr-args)
          :else
          (-> proc
            (update-in [:children]  into (args env))
            (update-in [:attrs]     into attr-args)))))))

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
      (analyze-node             hexp)
      (assert false (str "analyze: " hexp " is not a valid expression")))))

(def analyze-body   (comp funroll-body (partial map analyze)))
(def analyze-seq    (comp funroll-seq (partial map analyze)))

(def analyze-forms  (comp analyze-seq compile-forms))

;;;;;;;;;;;;;;;;;;;;;; Setup environment ;;;;;;;;;;;;;

(def prims
  {

   "foop"
   (fn [{:syms [hey] :as attr} args]
     (assoc
       (make-hexp "div")
       :children (vec args)
       :attrs (into {'asdf hey} attr)))

   })

(bind-global!
  (into {} (mapv #(vec [(first %) (make-prim-hexp (second %))]) prims)))

;;;;;;;;;;;;;;;;;;;;;; Evaluation ;;;;;;;;;;;;;;;;;;;;

(defn eval-forms [& forms]
  (remove nil? ((analyze-forms (hlisp.reader/read-forms (first forms))) {})))

(defn eval-string [s]
  (remove nil? ((analyze-forms (hlisp.reader/read-string s)) {})))


