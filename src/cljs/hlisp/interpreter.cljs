(ns hlisp.interpreter
  (:require
    [clojure.set])
  (:use
    [hlisp.util       :only [zipfn
                             funroll-body
                             funroll-seq]]
    [hlisp.compiler   :only [compile-forms
                             decompile-hexps]]
    [hlisp.hexp       :only [make-hexp
                             make-node-hexp
                             make-seq-hexp
                             make-text-hexp
                             make-prim-hexp
                             make-proc-hexp]]))

(declare analyze analyze-body analyze-seq apply* text-hexp? eval-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *global-env* (atom {}))
(def bind-env     into)
(def bind-global! (partial swap! *global-env* into))

(defn resolve-env [env name]
  (or (get env name) (get @*global-env* name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Syntactic Analysis ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (let [children    (elems (:children hexp))
          [name proc] ((zipfn [:tag analyze]) children)]
      (fn [env]
        (let [val (proc env)]
          (bind-global! {name val})
          nil)))))

(defn analyze-fn [hexp]
  (when (fn-hexp? hexp)
    (let [[c-params & body] (elems (:children hexp))
          params            (mapv :tag (:children c-params))
          attr-params       (:attrs c-params)
          proc              (analyze-body body)]
      (assert (seq body) "empty body")
      (fn [env]
        (make-proc-hexp attr-params params env proc)))))

(defn analyze-node [hexp]
  (let [name      (:tag hexp)
        attr-args (:attrs hexp)
        args      (analyze-seq (elems (:children hexp)))]
    (fn [env]
      (let [proc (resolve-env env name)
            argv (args env)]
        (assert proc (str "eval: unbound variable " name))
        (apply* proc attr-args (args env))))))

(defn analyze [hexp]
  (or
    (analyze-self-evaluating  hexp)
    (analyze-quoted           hexp)
    (analyze-def              hexp)
    (analyze-fn               hexp)
    (analyze-node             hexp)))

(def analyze-body   (comp funroll-body (partial map analyze)))
(def analyze-seq    (comp funroll-seq (partial map analyze)))
(def analyze-forms  (comp analyze-seq compile-forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Eval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eval* [forms]
  (decompile-hexps (remove nil? ((analyze-forms forms) {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Apply ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn apply-fn [hexp attr-args args]
  (when (and (seq args) (= :proc (:tag hexp)))
    (let [proc        (:proc        hexp)
          params      (:params      hexp)
          attr-params (:attr-params hexp)
          env         (bind-env (:env hexp) (parse-bindings params args))]
      (proc env))))

(defn apply-prim [hexp attr-args args]
  (when (and (seq args) (= :prim (:tag hexp)))
    ((:proc hexp) attr-args args)))

(defn apply-node [hexp attr-args args]
  (-> hexp
    (update-in [:children]  into args)
    (update-in [:attrs]     into attr-args)))

(defn apply* [hexp attr-args args]
  (or
    (apply-fn     hexp attr-args args)
    (apply-prim   hexp attr-args args)
    (apply-node   hexp attr-args args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bind-primitive! [prims]
  (bind-global!
    (into {} (mapv (comp vec (zipfn [str make-prim-hexp]))
                   (partition 2 prims)))))

