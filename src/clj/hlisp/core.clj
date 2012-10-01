(ns hlisp.core
  (:use
    [pl.danieljanus.tagsoup :only [parse tag attributes children]]
    [clojure.java.io        :only [file]]
    [hiccup.core            :only [html]]
    [hiccup.element         :only [javascript-tag]])
  (:require
    [clojure.string         :as string]
    [cljs.closure           :as cljsc]))

(def cljs-srcdir (file "src/cljs"))
(def html-srcdir (file "src/html"))
(def html-outdir (file "resources/public"))

(defn file-hidden? [f]
  (not= \. (first (.getName f))))

(defn file-ext [f]
  (let [fname (.getName f)]
    (subs fname (inc (.lastIndexOf fname ".")))))

(defn file-has-ext? [ext f]
  (= ext (file-ext f)))

(defn ext-filter [coll ext]
  (filter (partial file-has-ext? ext) coll))

(defn filter-tag [elems t]
  (filter #(= t (tag %)) elems))

(defn filter-attr [elems k v]
  (filter #(= v (k (attributes %))) elems))

(defn get-body [elem]
  (-> (children elem)
    (filter-tag :body)
    first))

(defn extract-script [elem]
  (-> (children elem)
    (filter-tag :head)
    first
    children
    (filter-tag :script)
    (filter-attr :type "text/hlisp")
    first
    (nth 2)))

(defn tagsoup->hlisp [elem]
  (if (string? elem)
    (list '$text elem)
    (let [[t attrs & kids] elem
          tag   (symbol (name t)) 
          kids  (map tagsoup->hlisp kids)]
      (list* tag attrs kids))))

(defn body->hlisp-str [body]
  (let [forms (cons 'list (drop 2 (-> (tagsoup->hlisp body))))]
    (str (list 'replace-body forms)) 
    ) 
  )

(defn build-page-hlisp [page]
  (let [s1  (extract-script page)
        s2  (-> (get-body page) body->hlisp-str)
        env (slurp "hlisp/env.cljs")]
    (string/join "\n" [env s1 s2])))

(defn get-hlisp-str [srcfile]
  (-> (parse srcfile) build-page-hlisp))

(defn tmpfile []
  (let [t (java.io.File/createTempFile "hlisp" ".cljs")]
    (.deleteOnExit t) 
    t))

(defn page-cljs-file []
  (let [t (java.io.File/createTempFile "hlisp" ".cljs" cljs-srcdir)]
    (.deleteOnExit t)
    t))

(defn compile-cljs [srcdir outfile & {:keys [externs]}]
  (cljsc/build srcdir {:optimizations :advanced
                       :externs externs
                       :output-to outfile}))

(defn compile-html [srcfile outfile cljsdir & {:keys [externs includes]}]
  (let [hl-src    (get-hlisp-str srcfile)
        html-src  (slurp srcfile)
        page-cljs (page-cljs-file)
        cljs-out  (tmpfile)]
    (spit page-cljs hl-src)
    (compile-cljs cljsdir (.getPath cljs-out) :externs externs)
    (let [compiled-str  (slurp cljs-out)
          incs          (string/join "\n" (map slurp includes))
          js-str        (string/join "\n" [incs compiled-str])
          js-tag        (html (javascript-tag js-str))]
      (.delete page-cljs) 
      (.delete cljs-out)
      (spit outfile
            (string/replace html-src "<!--__HLISP__-->" js-tag))
      )))







(comment
  
  (ext-filter (file-seq (file "src")) "cljs")

  (string/replace (slurp "src/html/index.html") #"</body>" "FOO\n</body>")

  (html (javascript-tag "window.foo = 1;")) 

  (compile-html "src/html/index.html"
                "resources/public/index.html"
                "src/cljs"
                :externs ["src/extern/jquery.js"]
                :includes ["src/jslib/jquery.js"]
                )

  (get-hlisp-str "src/html/index.html")
  
  (->
    (parse "src/html/index.html")
    build-page-hlisp
    )

  (->
    (parse "src/html/index.html")
    get-body
    body->hlisp-str
    ) 

  )

