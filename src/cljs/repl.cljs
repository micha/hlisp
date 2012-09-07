;(ns hlisp.repl)

(ns hlisp.repl
  (:require [clojure.browser.repl :as repl]))

(repl/connect "http://localhost:9000/repl")
