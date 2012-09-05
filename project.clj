(defproject cljs-starter "0.0.1"
  :description "ClojureScript starter project."
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.0.4"]]
  :plugins [[lein-cljsbuild "0.2.4"]
            [lein-ring "0.7.1"]]
  :cljsbuild {
    :builds [{:source-path "src/cljs"
              :compiler {:output-to "resources/public/js/main.js"
                         :optimizations :whitespace
                         :pretty-print true}}]}
  :ring {:handler example.routes/app})
