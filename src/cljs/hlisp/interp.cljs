(ns hlisp.interp)

(defn mkenv
  ([]
   "Global environment, no parent"
   {:parent nil :dict {}})

  ([env]
   "Environment frame, has parent env"
   {:parent env :dict {}}))

(defn analyze-error [expr]
  (throw (js/Error. "analyze error")))

(defn analyze [expr]
  (or
    ;(analyze-object       expr)
    ;(analyze-text         expr)
    ;(analyze-quoted       expr)
    ;(analyze-def          expr)
    ;(analyze-if           expr)
    ;(analyze-cond         expr)
    ;(analyze-begin        expr)
    ;(analyze-eval         expr)
    ;(analyze-apply        expr)
    ;(analyze-call         expr)
    ;(analyze-lambda       expr)
    ;(analyze-application  expr)
    ;(analyze-variable     expr)
    (analyze-error        expr)))

(defn eval [expr env]
  ((analyze expr) env))

