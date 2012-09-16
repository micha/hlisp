(ns hlisp.macros
  (:require [hlisp.reader :as reader]
            [hlisp.interp :as interp]))

(defmacro foo [& forms]
  (list 'quote '(+ 1 1)))

(defmacro hlisp [& forms]
  (js/console.log
    (.toString
      (interp/eval-forms forms))))
