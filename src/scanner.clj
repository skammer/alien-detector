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
(declare update-detections!)
(declare update-crosshair!)

(defonce conns (atom #{}))
(defonce invaders (atom #{}))
(defonce space-scan (atom nil))
(defonce current-detections (atom #{}))
(defonce crosshair (atom nil))

(add-watch current-detections :watcher
           (fn [key atom old-state new-state]
             (update-detections!)))

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

(defn initialize-zero [matrix]
  (let [h (count matrix)
        w (count (first matrix))]
    (take h (repeat (take w (repeat 0))))))

(defn get-matrix-chunk [matrix x-offset y-offset x-size y-size]
  (let [;; first crop off the top and left bits we don't need
        trimmed-top-left (ascii-parser/trim-around
                          matrix (dec y-offset) 0 0 (dec x-offset))
        ;; now trim the bottom rows
        necessary-rows (take y-size trimmed-top-left)]
    ;; and now just take the necessary row elements
    (map #(take x-size %) necessary-rows)))

(defn compare-chunks [template source]
  ;; Comparing template and source as if they are just a bunch of bits
  (try
   (let [template' (flatten template)
         source' (flatten source)

         matches (map-indexed
                  (fn [idx t]
                    (let [s (nth source' idx)]
                      ;; Potentially need to use some sort of tiered matching
                      ;; algorithm, taking into account the nature of the radar
                      ;; noise (maybe false positives are much more probable
                      ;; than false negatives? no idea)
                      (cond
                       ;; if template bit is high and it matches the source,
                       ;; it's the highest match possible
                       (and t (= t s)) 1
                       ;; if both template and detected bit are low and we
                       ;; expected low, also high match
                       (and (not t) (= t s)) 1
                       ;; Padded values match at 10% just in any case
                       (nil? s) 0.1
                       ;; all other cases we consider a miss
                       :else 0)))
                  template')

         template-bits (count template')
         matched-bits (apply + matches)]
     ;; normalizing
     ;; should ideally be 1 when all bits match cleanly
     (float (/ matched-bits template-bits)))
   ;; I'm expecting some weird out of bounds access errors at some poitn
   (catch Exception e
     (println "Error: " e)
     ;; return 0 score if failed
     0)))


(defn update-detection-matrix
  [detection-matrix offset-x offset-y size-x size-y]
  (let [h (count detection-matrix)
        w (count (first detection-matrix))

        flat-list (flatten detection-matrix)

        updated-list (map-indexed
                      (fn [idx el]
                        (let [current-row (int (/ idx w))
                              matching-row (and (<= offset-y current-row)
                                                (> (+ offset-y size-y) current-row))
                              current-col (mod idx w)
                              matching-col (and (<= offset-x current-col)
                                                (> (+ offset-x size-x) current-col))]
                          (if (and matching-row matching-col)
                            (inc el)
                            el)))
                      flat-list)]
    (partition w updated-list)))

(comment
 (= '((0 0 0 0) (0 0 1 1) (0 0 1 1) (0 0 0 0))
    (update-detection-matrix [[0 0 0 0] [0 0 0 0] [0 0 0 0] [0 0 0 0]] 2 1 2 2)))


(defn sum-matrices [& matrices]
  (let [w (count (first (first matrices)))
        flattened (map flatten matrices)
        sum (apply map + flattened)]
    (partition w sum)))

(comment
 (= [[2 2] [3 3] [4 4]]
    (sum-matrices [[1 1] [2 2] [3 3]]
                  [[1 1] [1 1] [1 1]])))

;; THIS IS SO SLOW!!!!
;; TODO: parallelize column loop as well
(defn detect [matrix invader]
  (let [invader-h (count invader)
        invader-w (count (first invader))

        ;; Add padding around the matrix so that we start comparing with the bottom
        ;; row of invader and end with top player, same with right and left.
        matrix' (ascii-parser/pad-around matrix (dec invader-h) (dec invader-w))

        detections (initialize-zero matrix')

        detection-matrices (pmap
                            (fn [row-idx]
                              ;; go row by row, comparing invader with matrix chunks
                              (reduce (fn [det col-idx]
                                        (let [chunk (get-matrix-chunk matrix' col-idx row-idx invader-w invader-h)
                                              similarity (compare-chunks invader chunk)
                                              threshold 0.8]
                                          (if (> similarity threshold)
                                            ;; TODO: only update high bits of the invader template
                                            ;; This way we can get cleaner output match image

                                            (do
                                             (swap! current-detections conj [invader col-idx row-idx])
                                             (update-detection-matrix det col-idx row-idx invader-w invader-h))
                                            det)))
                                      detections
                                      (range (- (count (first matrix')) invader-w))))
                            (range (- (count matrix') invader-h)))

        detection-matrix (apply sum-matrices detection-matrices)]

    (ascii-parser/trim-around detection-matrix (dec invader-h) (dec invader-w))))

(defn a []
  (detect radar-scan (first @invaders))
  "ok")

(comment
 (a))


;; TODO: use env var with fallback
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
       [:scan-canvas {:width 800 :height 400 :pixel-size 8 :base-color "#ffffff"
                      :pixels (mapv (fn [px] (if px 1 0)) (flatten radar-scan))}]

       [:pre {:class "opacity-40 text-sm mt-1"} (sha1-hash (str total-dots))]]))))

(def colors
  ["#FFD166"
   "#00F5FF"
   "#3AFF7A"
   "#FF4D6D"
   "#7B61FF"
   "#00E5FF"
   "#18FFFF"
   "#1DE9B6"
   "#00FF9C"
   "#39FF14"
   "#A4FF00"
   "#FFD300"
   "#FFB000"
   "#FF9100"
   "#FF6D00"
   "#FF3D81"
   "#FF1744"
   "#FF005D"
   "#E040FB"
   "#B388FF"
   "#7C4DFF"
   "#536DFE"
   "#448AFF"
   "#40C4FF"
   "#64FFDA"])

(defn get-color [idx]
  (let [total-colors (count colors)
        color-index (rem idx total-colors)]
    (nth colors color-index)))

(defn ->detections-overlay []
  (let [_ (reset! current-detections #{}) ;; clear old detections
        detections (pmap (partial detect radar-scan) @invaders)]
    (h/html
     (hc/compile
      [:div {:id "scan-overlay" :class "absolute top-0 left-0 opacity-50"}
       (map-indexed (fn [idx detection]
              [:scan-canvas {:class "absolute top-0 left-0"
                             :width 800 :height 400 :pixel-size 8
                             :base-color (rand-nth colors) ;; (get-color idx)
                             :pixels (flatten detection)}])
            detections)]))))

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

(defn ->detections []
  (h/html
   (hc/compile
    [:div {:id "identified-invaders" :class "flex flex-col flex-wrap overflow-y-scroll content-start"}
     (for [[invader x y] @current-detections]
       (let [h (count invader)
             w (count (first invader))
             x' (- x (int  (/ w 2)))
             y' (- y (int (/ h 2)))]
         [:p {:class "text-white cursor-pointer hover:text-amber-700 w-[200px] shrink"
              :data-on:click (d*/sse-post (str "/target/" x' "/" y'))}
          (str "✦ [" x' "," y' "]")]))])))

(defn update-detections! []
  (doseq [sse @conns]
    (try
      (d*/console-log! sse "Updated identified threats")
      (d*/patch-elements! sse (->detections))
      (catch Exception e
        (println "Error: " e)))))

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

(defn update-detections-overlay! [sse]
  (try
   (d*/console-log! sse "Scanned for invaders")
   (d*/patch-elements! sse (->detections-overlay))
   (catch Exception e
     (println "Error: " e))))

(defn update-state! []
  (update-invaders!)
  (update-scan!))

(defn connect [request]
  (->sse-response
   request
   {on-open
    (fn [sse]
      (swap! conns conj sse)
      (update-state!)

      (d*/execute-script! sse "loadingsound.play()")
      (d*/execute-script! sse "connectbutton.classList.add(\"hidden\"); ")
      (d*/execute-script! sse "scanbutton.classList.remove(\"hidden\")")
      #_(d*/with-open-sse sse))
    on-close
    (fn [sse status-code]
      (swap! conns disj sse)
      ; (d*/execute-script! sse "connectbutton.classList.remove(\"hidden\")")
      (println "Connection closed status: " status-code))}))


(defn scan [request]
  (->sse-response
   request
   {on-open
    (fn [sse]
      (d*/with-open-sse sse
        (d*/execute-script! sse "pingsound.pause()")
        (d*/execute-script! sse "pingsound.play()")
        (update-crosshair! sse {:x nil :y nil})
        (update-detections-overlay! sse)))
    on-close
    (fn [sse status-code])}))

(defn ->crosshair-overlay [{:keys [x y]}]
  (h/html
   (hc/compile
    [:div {:id "crosshair-overlay" :class "flex flex-col flex-wrap overflow-y-scroll"}
     (when x
       ;; Yes, read-string is definitely not safe, but we aren't exactly working with financial data here are we?
       [:div {:class ["absolute w-[8px] h-[400px] bg-fuchsia-600 top-0"
                      "animate-pulse"
                      (str "left-[" (* (read-string x) 8) "px]")]}])
     (when y
       [:div {:class ["absolute w-[800px] h-[8px] bg-fuchsia-600 left-0"
                      "animate-pulse"
                      (str "top-[" (* (read-string y) 8) "px]")]}])])))

(defn update-crosshair! [sse params]
  (try
   (d*/patch-elements! sse (->crosshair-overlay params))
   (catch Exception e
     (println "Error: " e))))

(defn set-crosshair [request]
  (->sse-response
   request
   {on-open
    (fn [sse]
      (d*/with-open-sse sse
        (update-crosshair! sse (:path-params request))))
    on-close
    (fn [sse status-code])}))

(def routes
  [["/" {:handler home}]
   ["/connect" {:handler    connect
                :middleware [reitit.ring.middleware.parameters/parameters-middleware]}]
   ["/scan" {:handler    scan
             :middleware [reitit.ring.middleware.parameters/parameters-middleware]}]
   ["/target/:x/:y" {:handler    set-crosshair
                     :middleware [reitit.ring.middleware.parameters/parameters-middleware]
                     :parameters {:path {:x int?
                                         :y int?}}}]

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
