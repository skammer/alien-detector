(ns radar
  (:require [ascii-parser]
            [clojure.string]))

(def NUM_INVADERS 3)

(defn place-bitmap-in-matrix
  [matrix offset-x offset-y bitmap & {:keys [only-white] :or {only-white true}}]
  (let [h (count matrix)
        w (count (first matrix))

        ;; vectorize the bitmap first
        bitmap (mapv vec bitmap)
        bh (count bitmap)
        bw (count (first bitmap))

        flat-list (flatten matrix)

        updated-list (map-indexed
                      (fn [idx el]
                        (let [current-row (int (/ idx w))
                              matching-row (and (<= offset-y current-row)
                                                (> (+ offset-y bh) current-row))
                              bitmap-row (- current-row offset-y)
                              current-col (mod idx w)
                              matching-col (and (<= offset-x current-col)
                                                (> (+ offset-x bw) current-col))
                              bitmap-col (- current-col offset-x)]
                          (if (and matching-row matching-col)
                            ;; Writing only white pixels
                            (let [pixel (get-in bitmap [bitmap-row bitmap-col] nil)]
                              ;; always skip nils
                              (if (nil? pixel)
                                el
                                (if only-white
                                  (or pixel el)
                                  pixel)))
                            el)))
                      flat-list)]
    (partition w updated-list)))

(defn place-invaders [matrix invaders]
  (let [h (count matrix)
        w (count (first matrix))]
    (reduce
     (fn [m invader]
       (reduce
        (fn [m' _]
          (let [offset-x (rand-int w)
                offset-y (rand-int h)]
            (place-bitmap-in-matrix m' offset-x offset-y invader)))
        m
        (range NUM_INVADERS)))
     matrix
     invaders)))

(def DEFAULT_NOISE_PROBABILITY 0.001)

(defn add-noise
  ([matrix] (add-noise matrix DEFAULT_NOISE_PROBABILITY true))
  ([matrix noise-probability] (add-noise matrix noise-probability true))
  ([matrix noise-probability noise-value]
  (let [w (count (first matrix))
        noisy-matrix (map
                      (fn [original-value]
                        (let [probability (rand)]
                          (if (> noise-probability probability)
                            noise-value
                            original-value)))
                      (flatten matrix))]
    (partition w noisy-matrix))))

(defn scan [h w invaders]
  (let [initial-matrix (partition w (take (* h w) (repeat 0)))
        invaders (or invaders #{[[]]})
        invader-h (apply max (map count invaders))
        invader-w (apply max (map (comp count first) invaders))
        padded-matrix (ascii-parser/pad-around initial-matrix (dec invader-h) (dec invader-w))

        resulting-matrix (-> padded-matrix
                             (place-invaders invaders)
                             (add-noise 0.1)
                             (add-noise 0.05 false))]
    (ascii-parser/trim-around resulting-matrix (dec invader-h) (dec invader-w))))

(defn render-pixel [pixel]
  (case pixel
    true "o"
    "-"))

(defn render-row [scan-row]
  (clojure.string/join "" (map render-pixel scan-row)))

(defn store-scan [scan-data]
  (let [scan-text (->> scan-data
                       (map render-row)
                       (clojure.string/join "\n"))]
    (spit "radar-scans/000.txt" scan-text)))
