(ns hlisp.dom
  (:require [jayq.core   :as jq]
            [jayq.util   :as ju]
            [hlisp.util  :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DOM -> hlisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (list 'value (-> node .-value))
    '()))

(defn build-attrs [node]
  (list
    (list
      (concat (mapcat (partial attr-kv node) (specified-attr-nodes node))
              (ie7-value node)))))

(defn dom->list [node]
  (let [tag (-> node .-nodeName .toLowerCase)] 
    (when (not= "script" tag)
      (let [attrs     (if (not= \# (first tag)) (build-attrs node) '())
            children  (if (branch? node)
                        (remove nil? (map dom->list (nodelist-seq (.-childNodes node)))) 
                        (list (.-nodeValue node)))] 
        (concat (list (symbol tag)) attrs children)))))

(defn read-dom [root]
  (list (dom->list root)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hlisp -> DOM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-elem [[tag text & _]]
  (let [t (str tag)]
    (condp = t
      "#text"     (-> js/document (.createTextNode  text))
      "#comment"  (-> js/document (.createComment   text))
      (-> js/document (.createElement t)))))

(defn write-dom [[tag attrs & children :as hexp]]
  (let [$elem (jq/$ (create-elem hexp))]
    (if (list? attrs)
      (-> $elem
        (jq/attr (into {} (vec (map vec (partition 2 (first attrs))))))
        (jq/append (mapv write-dom children)))
      $elem)))

