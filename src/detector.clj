(ns detector
  (:require [ascii-parser]))

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
                    (let [s (nth source' idx nil)]
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
     (/ matched-bits template-bits))
   ;; I'm expecting some weird out of bounds access errors at some point
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
(defn detect [detections-atom matrix invader]
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
                                              threshold 0.75]
                                          (if (> similarity threshold)
                                            ;; TODO: only update high bits of the invader template
                                            ;; This way we can get cleaner output match image
                                            (do
                                             (when detections-atom
                                               (swap! detections-atom conj [invader col-idx row-idx]))
                                             (update-detection-matrix det col-idx row-idx invader-w invader-h))
                                            det)))
                                      detections
                                      (range (- (count (first matrix')) (dec invader-w)))))
                            (range (- (count matrix') (dec invader-h))))

        detection-matrix (apply sum-matrices detection-matrices)]

    (ascii-parser/trim-around detection-matrix (dec invader-h) (dec invader-w))))
