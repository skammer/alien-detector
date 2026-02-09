(ns scanner
  (:require
   [ascii-parser]
   [charred.api]
   [clojure.java.io :as io]
   [dev.onionpancakes.chassis.compiler :as hc]
   [dev.onionpancakes.chassis.core :as h]
   [org.httpkit.server]
   [reitit.ring.middleware.parameters]
   [reitit.ring]
   [ring.util.response]
   [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open]]
   [starfederation.datastar.clojure.api :as d*]))


(def invader-directory (io/file "known-invaders/"))
(def invader-files (filter #(.isFile %) (file-seq invader-directory)))
(def invaders
  (map (fn [file]
         (ascii-parser/parse-file (.getAbsolutePath file)))
       invader-files))

(def scan-directory (io/file "radar-scans/"))
(def scan-files (filter #(.isFile %) (file-seq scan-directory)))
(def radar-scan
  (doall (ascii-parser/parse-file (first scan-files))) )

; (println radar-scan)


(def PORT 8099)

(def read-json (charred.api/parse-json-fn {:async? false :bufsize 1024}))

(defn get-signals [req]
  (-> req d*/get-signals read-json))

(def index-page
  (slurp (io/resource "index.html")))

(defn home [_]
  (-> index-page
      (ring.util.response/response)
      (ring.util.response/content-type "text/html")))

(def message "Hello, world!")

(defn ->frag [i]
  (h/html
   (hc/compile
    [:div {:id "message"}
     [:pre (str i)]
     [:div {:class "text-white text-[5px]"}
      (for [line radar-scan]
        (into
         [:div {:class "flex flex-row"}]
         (for [c line]
           [:div {:class ["h-2 w-2 outline-1 outline-black -outline-offset-1"
                          (if c "bg-white" "bg-black")
                          ]}]
           )))]])))

(defn scan [request]
  (let [d (-> request get-signals (get "delay") int)]
    (->sse-response
     request
     {on-open
      (fn [sse]
        (d*/with-open-sse sse
          (dotimes [i (count message)]
            (d*/patch-elements! sse (->frag i))
            (Thread/sleep d))))})))

(def routes
  [["/" {:handler home}]
   ["/scan" {:handler scan
                    :middleware [reitit.ring.middleware.parameters/parameters-middleware]}]
   ["/public/*" (reitit.ring/create-resource-handler)]])

(def router (reitit.ring/router routes))

(def handler (reitit.ring/ring-handler router))

;; ------------------------------------------------------------
;; Server
;; ------------------------------------------------------------
(defonce !server (atom nil))

(defn stop! []
  (if-let [s @!server]
    (do (org.httpkit.server/server-stop! s)
        (reset! !server nil))
    (throw (ex-info "Server not running" {}))))

(defn start! [handler opts]
  (when-not (nil? @!server)
    (stop!))
  (reset! !server
          (org.httpkit.server/run-server
           handler
           (merge {:port PORT}
                  opts
                  {:legacy-return-value? false}))))

(comment
  (stop!)
  (start! #'handler {})
  )

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------
(defn -main [& _]
  (start! #'handler {:port PORT})
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(do (stop!) (shutdown-agents)))))
