(ns hlisp.main
  (:require
    [jayq.core            :as   jq]
    [jayq.util            :as   ju]
    [goog.dom             :as   gdom])
  (:use
    [clojure.browser.dom  :only [log
                                 log-obj
                                 remove-children]]
    [hlisp.primitives     :only [prims]]
    [hlisp.dom            :only [read-dom
                                 write-dom
                                 load-remote-scripts]]
    [hlisp.reader         :only [read-forms
                                 read-string]]
    [hlisp.interpreter    :only [eval*
                                 bind-primitive!]]))

(js/console.time "load time")

(bind-primitive! prims)

(let [$body (jq/$ "body")]
  (doall
    (jq/empty $body)
    (map #(-> $body (jq/append %))
         (map write-dom
              (eval* (first (map read-string (load-remote-scripts)))))))) 

(js/console.timeEnd "load time")

