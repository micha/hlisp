(ns hlisp.main
  (:require
    [goog.dom             :as   gdom])
  (:use
    [clojure.browser.dom  :only [log
                                 log-obj
                                 remove-children]]
    [hlisp.primitives     :only [prims]]
    [hlisp.dom            :only [read-dom]]
    [hlisp.reader         :only [read-forms]]
    [hlisp.interpreter    :only [eval*
                                 bind-primitive!]]))

(bind-primitive! prims)

(def as-forms
  (comp log eval* read-forms))

(js/console.time "load time")

(as-forms
  '(

    (def f (fn [x & y] x))
    (def ttt
      (f
        (ul (li "one")
            (li "two")))) 

    (foop ((hey "omfg" yo "wheep")) p) 

    ))

(log (eval* (read-dom (-> js/document .-body)))) 

(gdom/removeChildren (-> js/document .-body)) 

(js/console.timeEnd "load time")

