(ns hlisp.macros)

(defmacro foo [& forms]
  ''(+ 1 1))

(defmacro defp [sym args & body]
  `(hlisp.interpreter/bind-primitive! [~(name sym) (fn ~args ~@body)]))

