(defproject cljs-test "0.1.0-SNAPSHOT"
  :description "Hlisp"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths  ["src/clj"]
  :plugins [[lein-ring "0.7.1"]]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [criterium "0.3.0"]
                 [hiccup "1.0.1"]
                 [compojure "1.0.4"]
                 [clj-tagsoup "0.3.0"]
                 [org.clojure/clojurescript "0.0-1450"]]
  :main hlisp.core
  :ring {:handler example.routes/app})
