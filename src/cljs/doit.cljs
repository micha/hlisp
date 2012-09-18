(ns hlisp.doit
  (:require
    [hlisp.reader :as reader]
    [hlisp.interpreter :as interp])
  (:require-macros
    [hlisp.macros :as macros]))

(defn as-forms [x]
  (js/console.log
    (.toString
      (interp/eval-forms x))))

(defn as-string [x]
  (js/console.log
    (.toString
      (interp/eval-string x))))

(as-forms
  '(

    (def f (fn [x & y] x))
    (def ttt (f (ul (li "one") (li "two")))) 
    (foop ((hey "omfg" yo "wheep")) p) 

    )) 
