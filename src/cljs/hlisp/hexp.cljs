(ns hlisp.hexp
  (:require [clojure.set]))

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

(defn make-hexp [tag]
  {:tag         tag
   :ids         (if (contains? html-tags tag) [(gensym)] [])
   :attrs       {}
   :children    []
   :text        ""
   :attr-params {}
   :params      []
   :env         {}
   :proc        nil
   :data        nil})

(defn make-node-hexp [tag attrs children]
  (assoc (make-hexp tag) :attrs attrs :children children))

(defn make-list-hexp [items]
  (make-node-hexp "list" {} (vec items)))

(defn make-text-hexp [tag text]
  (assoc (make-hexp tag) :text (str text)))

(defn make-prim-hexp [proc]
  (assoc (make-hexp :prim) :proc proc))

(defn make-proc-hexp [tag attr-params params env proc]
  (assoc (make-hexp tag)
         :attr-params attr-params
         :params      params
         :env         env
         :proc        proc))

(defn make-data-hexp [data]
  (assoc (make-hexp :data) :data data))

(defn make-quote-hexp [hexp]
  (make-node-hexp "quote" {} [hexp]))

