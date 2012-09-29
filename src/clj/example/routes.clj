(ns example.routes
  (:use compojure.core)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(defroutes main-routes
  (route/resources "/")
  (route/not-found "Page not found"))

(defn wrap-dir-index [handler]
  (fn [req]
    (handler
     (update-in req [:uri]
                #(if (= "/" %) "/index.html" %)))))

(defn wrap-content-type [handler type-map]
  (fn [req]
    (let [resp (handler req)
          uri  (:uri req)
          ext  (clojure.string/replace uri #".*\.([^\.]+)$" "$1")
          typ  (or (and (not= uri ext) (get type-map ext))
                   (get-in resp [:headers "Content-Type"]))]
      (assoc-in resp [:headers "Content-Type"] typ))))

(def app
  (-> (handler/site main-routes)
      (wrap-content-type {"hl" "text/plain"}) 
      wrap-dir-index))
