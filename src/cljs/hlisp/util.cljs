(ns hlisp.util)

(def zipfn (partial partial map #(apply %1 %&)))

(def trans (partial apply map list))

(def funroll-body
  (partial reduce #(fn [& args] (apply %1 args) (apply %2 args))))

(defn funroll-seq [procs]
  (fn [& args] (map #(apply % args) procs)))

(defn tee [arg]
  (js/console.log ";;" (str arg))
  arg)

