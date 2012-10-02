(ns hlisp.core
  (:use
    [criterium.core         :only [time-body]]
    [hlisp.watchdir         :only [watch-dir-ext process-b]]
    [hlisp.colors           :only [style pr-ok]]
    [pl.danieljanus.tagsoup :only [parse tag attributes children]]
    [clojure.java.io        :only [file]]
    [clojure.stacktrace     :only [print-stack-trace]]
    [clojure.pprint         :only [pprint]]
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

(defn munge-path [path]
  (-> (string/replace path "_" "__") (string/replace "/" "_")))

(defn extract-script [elem]
  (let [forms-str           (-> (children elem)
                              (filter-tag :head)
                              first
                              children
                              (filter-tag :script)
                              (filter-attr :type "text/hlisp")
                              first
                              (nth 2))
        [ns-decl & forms]   (read-string (str "(" forms-str ")"))
        prelude-str         (slurp "src/template/prelude.cljs")
        prelude             (read-string (str "(" prelude-str ")"))
        complete            (concat (list ns-decl) prelude forms)]
    (string/join "\n" (map pr-str complete))))

(defn tagsoup->hlisp [elem]
  (if (string? elem)
    (list '$text elem)
    (let [[t attrs & kids] elem
          tag   (symbol (name t)) 
          kids  (map tagsoup->hlisp kids)]
      (list* tag attrs kids))))

(defn body->hlisp-str [body]
  (let [forms (cons 'list (drop 2 (-> (tagsoup->hlisp body))))]
    (str (list 'hlisp.env/replace-body forms))))

(defn build-page-hlisp [page]
  (let [s1  (extract-script page)
        s2  (-> (get-body page) body->hlisp-str)]
    (string/join "\n" [s1 s2])))

(defn get-hlisp-str [srcfile]
  (-> (parse srcfile) build-page-hlisp))

(defn tmpfile []
  (let [t (java.io.File/createTempFile "hlisp" ".cljs")]
    (.deleteOnExit t) 
    t))

(defn page-cljs-file [srcfile cljsdir]
  (str cljsdir "/___" (munge-path srcfile) ".cljs"))

(defn compile-cljs [srcdir outfile & {:keys [optimizations externs]}]
  (cljsc/build srcdir {:optimizations optimizations
                       :externs externs
                       :output-to outfile}))

(defn compile-html [srcfile outfile cljsdir & {:keys [optimizations externs includes]}]
  (let [hl-src    (get-hlisp-str srcfile)
        html-src  (slurp srcfile)
        page-cljs (str cljsdir "/___page___.cljs")
        cljs-out  (tmpfile)]
    (spit page-cljs hl-src)
    (compile-cljs cljsdir (.getPath cljs-out) :optimizations optimizations
                                              :externs externs)
    (let [compiled-str  (slurp cljs-out)
          incs          (string/join "\n" (map slurp includes))
          js-str        (string/join
                          ["\nvar CLOSURE_NO_DEPS = true;\n" incs compiled-str])
          js-tag        (html (javascript-tag js-str))]
      (.delete cljs-out)
      (spit outfile
            (string/replace html-src "<!--__HLISP__-->" js-tag)))))

(defn outfile [srcdir outdir infile]
  (let [rel (subs infile (inc (count srcdir)))]
    (str outdir "/" rel)))

(defn elapsed-ms [f & args]
  (int (/ (first (time-body (apply f args)))
          1000000)))

(defn compile-watch [srcdir outdir cljsdir & {:keys [optimizations externs includes]}]
  (->>
    (watch-dir-ext srcdir "html" 100)
    (process-b
      (fn [infile]
        (try
          (let [out (outfile srcdir outdir (.getPath infile))]
            (println (style (java.util.Date.) :blue)
                     "\n   "
                     (style infile :bold-blue)
                     (style "->" :blue)
                     (style out :bold-blue)) 
            (flush)
            (pr-ok
              true
              (format
                "    Elapsed time: %d ms.\n"
                (elapsed-ms
                  compile-html infile out cljsdir :optimizations optimizations
                                                  :externs externs
                                                  :includes includes)))
            (flush)
            true)
          (catch Throwable e
            (pr-ok false (format "%s" (with-out-str (print-stack-trace e))))
            (flush)
            false))))))

(defn -main [opt]
  (do
    (compile-watch "src/html" "resources/public" "src/cljs"
                   :optimizations (keyword opt)
                   :externs ["src/extern/jquery.js"]
                   :includes ["src/jslib/jquery.js"])
    (println "\nCompiling html files in 'src/html' to 'resources/public'.")))


