(ns hlisp.dom) 

(def node-types
  {1 :element})

(defn branch? [node]
  (and (not (nil? node))
       (= :element (node-types (-> node .-nodeType)))))

(defn nodelist-seq [nodelist]
  (when nodelist
    (let [len (.-length nodelist)]
      (map #(.item nodelist %) (range 0 len)))))

(defn specified-attr-nodes [node]
  (filter #(.-specified %) (nodelist-seq (.-attributes node))))

(defn attr-kv [node attr-node]
  (let [k (symbol (-> attr-node .-nodeName .toLowerCase)) 
        v (if (= k "style")
            (-> node .-style .-cssText)
            (.-nodeValue attr-node))]
    (list k v)))

(defn build-attrs [node]
  (list (mapcat (partial attr-kv node) (specified-attr-nodes node))))

(defn dom->list [node]
  (list* (symbol (-> node .-nodeName .toLowerCase)) 
         (build-attrs node)
         (if (branch? node)
           (map dom->list (nodelist-seq (.-childNodes node)))
           (list (.-nodeValue node)))))
