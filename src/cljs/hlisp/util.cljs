(ns hlisp.util)

(def zipfn (partial partial map #(apply %1 %&)))

(def funroll-body
  (partial reduce #(fn [& args] (apply %1 args) (apply %2 args))))

(defn funroll-seq [procs]
  (fn [& args] (map #(apply % args) procs)))

(defn clj->js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings. --http://github.com/ibdknox/jayq"
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.-strobj (reduce (fn [m [k v]]
               (assoc m (clj->js k) (clj->js v))) {} x))
    (coll? x) (apply array (map clj->js x))
    :else x))
