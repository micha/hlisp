(ns hlisp.dom
  (:require [goog.dom             :as gdom]
            [clojure.browser.dom  :as dom]
            [hlisp.util           :as util]))

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
    (concat (list (symbol tag)) attrs children)))

(defn read-dom [root]
  (list (dom->list root)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hlisp -> DOM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-elem [hexp]
  (let [{:keys [tag text]} hexp, tagstr (str tag)]
    (condp = tagstr
      "#text"     (-> js/document (.createTextNode  text))
      "#comment"  (-> js/document (.createComment   text))
      (-> js/document (.createElement tagstr)))))

(defn write-dom [hexp]
  (let [{:keys [attrs children]} hexp
        js-attrs  (util/clj->js attrs)
        elem      (-> (create-elem hexp) (gdom/setProperties js-attrs))]
    (if (seq children)
      elem
      (apply dom/append elem (map write-dom children)))
    
    ))





