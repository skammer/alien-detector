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
   [starfederation.datastar.clojure.adapter.http-kit :refer [->sse-response on-open on-close]]
   [starfederation.datastar.clojure.api :as d*]
   [nextjournal.beholder :as beholder])
  (:import [java.security MessageDigest]))

(defn sha1-hash [input]
  (let [digest (MessageDigest/getInstance "SHA-1")]
    (.update digest (.getBytes input))
    (let [hash-bytes (.digest digest)]
      (apply str (map #(format "%02x" %) hash-bytes)))))

(declare update-invaders!)

(defonce conns (atom #{}))
(defonce invaders (atom #{}))
(defonce space-scan (atom nil))

(defn parse-invaders []
  (let [invader-directory (io/file "known-invaders/")
        invader-files (filter #(.isFile %) (file-seq invader-directory))
        scanned-invaders (map (fn [file]
                                (ascii-parser/parse-file (.getAbsolutePath file)))
                              invader-files)]
    (println "Updated invaders database: " (count scanned-invaders))
    (reset! invaders (set scanned-invaders))))


(def scan-directory (io/file "radar-scans/"))
(def scan-files (filter #(.isFile %) (file-seq scan-directory)))
(def radar-scan
  (doall (ascii-parser/parse-file (first scan-files))) )


;; Initialize invaders DB
(parse-invaders)

;; And keep waching the directory for new definitions
(def invader-watcher
  (beholder/watch
   (fn [change]
     ; (println change)
     (parse-invaders)
     (update-invaders!))
   "known-invaders"))

(comment
 (beholder/stop invader-watcher))

;; Scan:
;; * pad
;; * start matching
;; * compare threshold
;; * mark all pixels as match if above threshold
;; * draw heat map
;; * find peaks, save as probable locations


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

(defn ->scanner []
  (let [total-dots (->> radar-scan
                       flatten
                       (filter true?)
                       count)]
    (h/html
     (hc/compile
      [:div {:id "scanner" :class "w-[800px] h-[400px]"}
       [:div {:class "text-white text-[5px]"}
        (for [line radar-scan]
          (into
           [:div {:class "flex flex-row"}]
           (for [c line]
             [:div {:class ["h-2 w-2 outline-1 outline-black -outline-offset-1"
                            (if c "bg-white" "bg-black") ]}])))]
       [:pre {:class "opacity-40 text-sm mt-1"} (sha1-hash (str total-dots))]]))))

(defn ->known-invaders []
  (h/html
   (hc/compile
    [:div {:id "known-invaders" :class "flex flex-row flex-wrap overflow-y-scroll"}
     (for [invader @invaders]
       [:div {:class "text-white text-[5px] p-2"}
        (for [line invader]
          (into
           [:div {:class "flex flex-row"}]
           (for [c line]
             [:div {:class ["h-2 w-2 outline-1 outline-black -outline-offset-1"
                            (if c "bg-white" "bg-black")]}])))])])))

(defn update-invaders! []
  (doseq [sse @conns]
    (try
      (d*/console-log! sse "Updated threat database")
      (d*/patch-elements! sse (->known-invaders))
      (catch Exception e
        (println "Error: " e)))))

(defn update-scan! []
  (doseq [sse @conns]
    (try
      (d*/console-log! sse "Updated space scan")
      (d*/patch-elements! sse (->scanner))
      (catch Exception e
        (println "Error: " e)))))

(defn update-state! []
  (update-invaders!)
  (update-scan!))

(defn connect [request]
  (let [d (-> request get-signals (get "delay") int)]
    (->sse-response
     request
     {on-open
      (fn [sse]
        (swap! conns conj sse)
        (update-state!)

        (d*/execute-script! sse "loadingsound.play()")
        (d*/execute-script! sse "connectbutton.classList.add(\"hidden\"); ")
        (d*/execute-script! sse "scanbutton.classList.remove(\"hidden\")"))

        #_(d*/with-open-sse sse
          )
      on-close
      (fn [sse status-code]
        (swap! conns disj sse)
        ; (d*/execute-script! sse "connectbutton.classList.remove(\"hidden\")")
        (println "Connection closed status: " status-code))})))


(defn scan [request]
  (->sse-response
   request
   {on-open
    (fn [sse]
      (d*/execute-script! sse "pingsound.play()")
      )
    on-close
    (fn [sse status-code])}))

(def routes
  [["/" {:handler home}]
   ["/connect" {:handler    connect
                :middleware [reitit.ring.middleware.parameters/parameters-middleware]}]
   ["/scan" {:handler    scan
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
  (start! #'handler {}))

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------
(defn -main [& _]
  (start! #'handler {:port PORT})
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. #(do (stop!) (shutdown-agents)))))
