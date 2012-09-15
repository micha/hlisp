(ns hlisp.zip
  (:require [clojure.zip :as z])) 

(defn append-children! [node children]
  (doseq [child children]
    (.appendChild node child))) 

(defn dom-zip [dom-node]
  (z/zipper #(and (not (nil? %))
                  (= :element (node-types (-> % .-nodeType))))
            #(nodelist-seq (.-childNodes %))
            #(doto %1 (append-children! %2))
            dom-node)) 

(defn dom-seq
  [loc]
  (filter identity
          (lazy-seq
           (when-not (z/end? loc)
             (cons (z/node loc) (dom-seq (z/next loc))))))) 

(defn postwalk-replace [loc f]
  (if-not (z/end? loc)
    (recur (-> loc #(z/replace % (f (z/node %))) z/next) f)
    (-> loc z/root z/node)))

(defn doit []
  (eval '(js/alert "hi there")))

