(ns hlisp.interp)

(def html-tags
  "Union of HTML 4.01 and HTML 5 tags"
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

(def hlisp-object-tags
    "HLisp-specific self-evaluating symbols"
    #{ "#text" "#comment" "val" "list" "hash" "true" "false" "nil" "null" "fmeta" })

(defn mkenv
  ([]
   "Global environment, no parent"
   {:parent nil :dict {}})

  ([env]
   "Environment frame, has parent env"
   {:parent env :dict {}}))

(defn self-evaluating? [expr]
  (let [type (:type expr)]
    (or (contains? html-tags type)
        (contains? hlisp-object-tags type)
        (not (hlisp.reader/elem-tag? type)))))

(defn analyze-self-evaluating [expr]
  (if (self-evaluating? expr)
    (let [analyzed-chlds (map analyze (:chld expr))]
      (fn [env]
        (assoc expr :chld (mapv #(% env) analyzed-chlds))))))

(defn text-expr? [expr]
  (= "txt" (:type expr)))

(defn analyze-text-expr [expr]
  (if (text-expr? expr)
    (let [s (apply str (map :text (:chld expr)))
          n (hlisp.reader/text-node s)]
      (fn [env] n))))

(defn quoted-expr? [expr]
  (= "quote" (:type expr)))

(defn analyze-quoted-expr [expr]
  (if (quoted-expr? expr)
    (fn [env]
      (first (:chld expr)))))

(defn analyze-error [expr]
  (throw (js/Error. "analyze error")))

(defn analyze [expr]
  (or
    (analyze-self-evaluating  expr)
    (analyze-text-expr        expr)
    (analyze-quoted-expr      expr)
    ;(analyze-def              expr)
    ;(analyze-if               expr)
    ;(analyze-cond             expr)
    ;(analyze-begin            expr)
    ;(analyze-eval             expr)
    ;(analyze-apply            expr)
    ;(analyze-call             expr)
    ;(analyze-lambda           expr)
    ;(analyze-application      expr)
    ;(analyze-variable         expr)
    (analyze-error            expr)))

(defn eval [expr env]
  ((analyze expr) env))

(defn doit [s]
  (eval (hlisp.reader/build-exp (hlisp.reader/normalize-exp s))))
