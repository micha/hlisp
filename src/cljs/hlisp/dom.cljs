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
        v (condp = k
            "style" (-> node .-style .-cssText)
            (.-nodeValue attr-node))]
    (list k v)))

(defn ie7-value [node]
  (if-let [v (and (branch? node) (-> node .-value))]
    (list "value" (-> node .-value))
    '()))

(defn build-attrs [node]
  (list
    (list
      (concat (mapcat (partial attr-kv node) (specified-attr-nodes node))
              (ie7-value node)))))

(defn dom->list [node]
  (let [tag       (-> node .-nodeName .toLowerCase)
        attrs     (if (not= \# (first tag)) (build-attrs node) '())
        children  (if (branch? node)
                    (map dom->list (nodelist-seq (.-childNodes node)))
                    (list (.-nodeValue node)))]
    (concat (list tag) attrs children)))
