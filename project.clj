(defproject cljs-starter "0.0.1"
  :description "Hlisp"
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.0.4"]]
  :plugins [[lein-cljsbuild "0.2.7"]
            [lein-ring "0.7.1"]]
  :cljsbuild {
    :builds [{:source-path "src/cljs"
              :compiler {:output-to "resources/public/js/hlisp.js"
                         :optimizations :whitespace
                         :pretty-print true}}]}
  :ring {:handler example.routes/app})
