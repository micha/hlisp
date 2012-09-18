(ns hlisp.main
  (:use [hlisp.primitives   :only [prims]]
        [hlisp.interpreter  :only [bind-primitive!
                                   eval-forms
                                   eval-string]]))

(bind-primitive! prims)

(defn as-forms [x]
  (js/console.log (.toString (eval-forms x)))) 

(defn as-string [x]
  (js/console.log (.toString (eval-string x)))) 

(as-forms
  '(

    (def f (fn [x & y] x))
    (def ttt (f (ul (li "one") (li "two")))) 
    (foop ((hey "omfg" yo "wheep")) p) 

    ))
