(ns hlisp.main
  (:require
    [hlisp.primitives     :as   p]
    [flapjax.core         :as   fj]
    [jayq.core            :as   jq]
    [goog.dom             :as   gdom])
  (:use
    [jayq.util            :only [clj->js]]
    [hlisp.util           :only [tee]]
    [hlisp.dom            :only [read-dom
                                 write-dom]]
    [hlisp.reader         :only [read-forms
                                 read-string]]
    [hlisp.interpreter    :only [eval*
                                 eval-string*
                                 bind-primitive!]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hlisp -> DOM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def xhr-opts
  {:async     false
   :dataType  "text"
   :type      "GET"
   })

(defn status-ok? [status]
  (and (>= status 200) (< status 300)))

(defn xhr [uri & {:keys [opt]}] 
   (let [ret      (.ajax js/jQuery (str uri) (clj->js (into xhr-opts opt)))
         status   (.-status       ret)
         message  (.-statusText   ret)
         text     (.-responseText ret)]
     (assert (status-ok? status) (str status " " message))
     text))

(defn load-remote-scripts []
  (mapv #(if (.is % "[src]")
           (xhr (jq/attr % "src"))
           (jq/text %)) 
        (-> (jq/$ "head") (jq/find "script[type='text/hlisp']")))) 

(defn init []
  (let [body      (-> js/document .-body)
        $body     (jq/$ body)
        scrp-src  (map read-string (load-remote-scripts))
        body-src  (drop 2 (vec (first (read-dom body))))]
    (js/console.time "init")
    (jq/empty $body)
    (mapv #(-> $body (jq/append %))
          (concat (mapcat #(map write-dom (eval* %)) scrp-src) 
                  (map write-dom (eval* body-src))))
    (fj/initialize)
    (js/console.timeEnd "init")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hlisp -> DOM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-primitive! p/prims)

(bind-primitive! ["dom-assoc!"    p/dom-assoc!
                  "dom-dissoc!"   p/dom-dissoc!
                  "dom-css!"      p/dom-css!
                  "map"           p/x-map
                  "mapcat"        p/x-mapcat
                  "partition"     p/x-partition
                  "sync-e"        p/sync-e
                  ])

(set! js/hl eval-string*)

(jq/document-ready init)


