{:html-src    "src/html"
 :cljs-src    "src/cljs"
 :html-out    "resources/public"
 :prelude     "src/template/prelude.cljs"
 :includes    ["src/jslib/jquery.js"]
 :cljsc-opts  {:optimizations  :advanced
               :externs        ["src/extern/jquery.js"]}} 
