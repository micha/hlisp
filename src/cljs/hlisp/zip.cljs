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

;;;;;;;;;;;;;;;;;;;;

(def node-types
  {1 :element
   3 :text})

(defn branch? [node]
  (and (not (nil? node))
       (= :element (node-types (-> node .-nodeType)))))

(defn nodelist-seq [nodelist]
  (when nodelist
    (let [len (.-length nodelist)]
      (map #(.item nodelist %) (range 0 len)))))

(defn build-attrs [node]
  (let [attr-kv (juxt (comp symbol #(.toLowerCase %) #(.-nodeName %)) #(.-nodeValue %))]
    (list (mapcat attr-kv (nodelist-seq (.-attributes node))))))

(defn node->seq [node]
  (list* (-> node .-nodeName .toLowerCase)
         (build-attrs node)
         (if (branch? node)
           (map node->seq (nodelist-seq (.-childNodes node)))
           (list (.-nodeValue node)))))