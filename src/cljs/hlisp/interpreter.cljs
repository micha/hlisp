(ns hlisp.interpreter
  (:require
    [clojure.set])
  (:use
    [hlisp.util       :only [zipfn
                             tee
                             funroll-body
                             funroll-seq]]
    [hlisp.compiler   :only [compile-forms
                             compile-form
                             dc
                             dcs
                             decompile-hexp
                             decompile-hexps]]
    [hlisp.reader     :only [read-forms
                             read-form
                             read-string]]
    [hlisp.hexp       :only [html-tags
                             html-text-tags
                             make-hexp
                             make-node-hexp
                             make-data-hexp
                             make-list-hexp
                             make-text-hexp
                             make-quote-hexp
                             make-prim-hexp
                             make-proc-hexp]]))

(declare analyze analyze-all analyze-body analyze-seq apply* text-hexp?
         data-hexp? syntax-quote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *global-env* (atom {}))
(def bind-env     into)
(def bind-global! (partial swap! *global-env* into))

(defn resolve-env [env name]
  (or (get env name) (get @*global-env* name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Syntactic Analysis ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def hlisp-tags
  #{ "list" })

(def hlisp-boxed-tags
  #{ "val:str" "val:num" "val:seq" "val:vec" "val:set" "val:map" "val:true"
     "val:false" "val:nil" })

(def self-evaluating-tags
  (clojure.set/union html-tags html-text-tags hlisp-tags hlisp-boxed-tags))

(defn elems [hexps]
  (remove text-hexp? hexps))

(defn text-hexp? [hexp]
  (= \# (first (:tag hexp))))

(defn self-evaluating-hexp? [hexp]
  (or (contains? self-evaluating-tags (:tag hexp))
      (text-hexp? hexp)
      (data-hexp? hexp)))

(defn has-tag? [tag hexp]
  (= tag (:tag hexp)))

(def quoted-hexp?         (partial has-tag? "quote"))
(def syntax-quoted-hexp?  (partial has-tag? "syntax-quote"))
(def defvalues-hexp?      (partial has-tag? "defvalues"))
(def def-hexp?            (partial has-tag? "def"))
(def if-hexp?             (partial has-tag? "if"))
(def apply-hexp?          (partial has-tag? "apply"))
(def eval-hexp?           (partial has-tag? "eval"))
(def do-hexp?             (partial has-tag? "do"))
(def macro-hexp?          (partial has-tag? "macro"))
(def data-hexp?           (partial has-tag? :data))
(def let-hexp?            (partial has-tag? "let"))
(def fn-hexp?             (partial has-tag? "fn"))

(defn truthy-hexp? [hexp]
  (let [{:keys [tag data]} hexp]
    (or (not (= :data tag))
        (and (not= nil data) (not= false data)))))

(defn analyze-quoted [hexp]
  (when (quoted-hexp? hexp)
    (fn [env]
      (first (elems (:children hexp))))))

(defn analyze-syntax-quoted [hexp]
  (when (syntax-quoted-hexp? hexp)
    (let [form (first (elems (:children hexp)))]
      (analyze (first (:children (syntax-quote form)))))))

(defn analyze-defvalues [hexp]
  (when (defvalues-hexp? hexp)
    (let [args    (elems (:children hexp))
          syms    (butlast args)
          proc    (analyze (last args))
          ks      (map :tag syms)]
      (fn [env]
        (let [vs (elems (:children (proc env)))]
          (doall (map #(bind-global! {%1 %2}) ks vs)) 
          (make-data-hexp nil))))))

(defn analyze-def [hexp]
  (when (def-hexp? hexp)
    (let [[{:keys [tag]} expr]  (elems (:children hexp))
          proc                  (analyze expr)
          bind-now              (macro-hexp? expr)]
      (when bind-now
        (bind-global! {tag (proc {})}))
      (fn [env]
        (when-not bind-now
          (bind-global! {tag (proc env)})) 
        (make-data-hexp nil)))))

(defn analyze-if [hexp]
  (when (if-hexp? hexp)
    (let [[pred consq alt] (analyze-all (elems (:children hexp)))]
      (fn [env]
        (if (truthy-hexp? (pred env))
          (consq env)
          (if alt
            (alt env)
            (make-data-hexp nil)))))))

(defn analyze-eval [hexp]
  (when (eval-hexp? hexp)
    (let [proc (analyze (first (elems (:children hexp))))]
      (fn [env]
        ((analyze (proc env)) {})))))

(defn analyze-apply [hexp]
  (when (apply-hexp? hexp)
    (let [[f v]     (elems (:children hexp))
          proc      (analyze f)
          args      (analyze v)]
      (fn [env]
        (let [p (proc env)
              a (args env)]
          (apply* p {} (elems (:children a))))))))

(defn analyze-do [hexp]
  (when (do-hexp? hexp)
    (analyze-body (elems (:children hexp)))))

(defn analyze-let [hexp]
  (when (let-hexp? hexp)
    (let [{:keys [children]} hexp
          [{bind-pairs :children} & body] (elems children)
          nbind (count bind-pairs)]
      (assert (even? nbind) "odd number of bindings for let")
      (if (< 0 nbind)
        (let [wrap-param  [(make-list-hexp [(first bind-pairs)])] 
              wrap-arg    [(second bind-pairs)]
              next-bind   [(make-list-hexp (vec (drop 2 bind-pairs)))] 
              next-let    [(make-node-hexp "let" {} (into next-bind (vec body)))] 
              wrap-fn     [(make-node-hexp "fn" {} (into wrap-param next-let))]]
          (analyze (make-node-hexp "call" {} (into wrap-fn wrap-arg))))
        (analyze-body body)))))

(defn analyze-fn [hexp]
  (when-let [fn-type (cond (fn-hexp? hexp)    :proc
                           (macro-hexp? hexp) :macro)]
    (let [[c-params & body] (elems (:children hexp))
          params            (mapv :tag (:children c-params))
          attr-params       (:attrs c-params)
          proc              (analyze-body body)]
      (assert (seq body) "empty body")
      (fn [env]
        (make-proc-hexp fn-type attr-params params env proc)))))

(defn analyze-node [hexp]
  (let [{:keys [tag attrs children]} hexp
        form (resolve-env {} tag)] 
    (if (= :macro (:tag form))
      (analyze (apply* form attrs children))
      (let [args1 (analyze-all children)
            args2 (funroll-seq args1)]
        (fn [env]
          (let [proc (resolve-env env tag)]
            (cond
              proc
              (apply* proc attrs (args2 env))

              (self-evaluating-hexp? hexp)
              (assoc hexp :children (mapv #(% env) args1)))))))))

(defn analyze [hexp]
  (or
    (analyze-quoted           hexp)
    (analyze-syntax-quoted    hexp)
    (analyze-defvalues        hexp)
    (analyze-def              hexp)
    (analyze-if               hexp)
    (analyze-eval             hexp)
    (analyze-apply            hexp)
    (analyze-do               hexp)
    (analyze-let              hexp)
    (analyze-fn               hexp)
    (analyze-node             hexp)
    (assert false (str "unbound variable " (:tag hexp)))))

(def analyze-all    (partial map analyze))
(def analyze-body   (comp funroll-body analyze-all))
(def analyze-seq    (comp funroll-seq analyze-all))
(def analyze-forms  (comp analyze-seq compile-forms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Syntax Quoting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn syntax-quote-list [{:keys [tag children] :as hexp}]
  (let [wrap (make-hexp "concat")
        head [(make-quote-hexp (make-hexp tag))] 
        tail (mapv syntax-quote (elems children))]
    (make-node-hexp "concat" {} (into head tail))))

(defn syntax-quote [{:keys [tag children] :as hexp}]
  (let [child-args (elems children)]
    (cond
      (= "unquote-splicing" tag)
      (first child-args)

      (= "unquote" tag) 
      (make-list-hexp [(first child-args)])

      (not (seq child-args)) 
      (make-list-hexp [(make-quote-hexp hexp)])

      :else
      (make-list-hexp [(syntax-quote-list hexp)]))))

(defn sq-test [form]
  (tee dc "~~[st]~~" (first (elems (:children (syntax-quote (compile-form (read-form form)))))))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Eval ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn filter-ids [hexps]
  (map #(if (self-evaluating-hexp? %) % (assoc % :ids [])) hexps))

(defn eval* [forms]
  (decompile-hexps (filter-ids ((analyze-forms forms) {}))))

(defn eval-string* [s]
  (tee dcs ">>" (filter-ids ((analyze-forms (read-string s)) {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Apply ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn parse-bindings [params args]
  (let [k (first params)
        v (first args)]
    (cond
      (= "&" k)
      (parse-bindings (rest params) (list (make-list-hexp args)))
      (and k v)
      (into {k v} (parse-bindings (rest params) (rest args)))
      (or k v)
      (assert false "arity mismatch"))))

(defn apply* [hexp attr-args args]
  (let [{:keys [tag attrs proc params attr-params env]} hexp]
    (cond
      (or (string? tag) (nil? (seq args))) 
      (-> hexp
        (update-in [:children]  into args)
        (update-in [:attrs]     into attr-args))

      (= :prim tag)
      (proc attr-args (elems args))

      (or (= :macro tag) (= :proc tag)) 
      (proc (bind-env env (parse-bindings params (elems args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bindp! [name f]
  (bind-global! {(str name) (make-prim-hexp f)}))

(defn bind-primitive! [prims]
  (bind-global!
    (into {} (mapv (comp vec (zipfn [str make-prim-hexp]))
                   (partition 2 prims)))))

