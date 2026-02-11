(ns ascii-parser
  (:require [clojure.java.io]
            [clojure.tools.logging :as log]))

(defn normalize
  "Makes sure all rows are of equal length, pads missing data with nils"
  [matrix]
  (let [lengths (map count matrix)]
    (if (apply = lengths)
      matrix
      (let [max-len (apply max lengths)]
        (log/warn "Uneven matrix" {:matrix matrix})
        (map #(take max-len (concat % (repeat nil))) matrix)))))

(defn parse-line
  "nil is missing data, true is detection, false if no detection"
  [line]
  (for [pixel line]
    (case pixel
      \- false
      \o true
      nil)))

(defn parse-input [lines]
  (normalize
   (map parse-line lines)))

(defn parse-file [file-path]
  (with-open [reader (clojure.java.io/reader file-path)]
    (parse-input (doall (line-seq reader)))))

(defn pad-y [matrix pad-t pad-b]
  (let [w (count (first matrix))
        prefix (take pad-t (repeat (take w (repeat nil))))
        suffix (take pad-b (repeat (take w (repeat nil))))]
    (concat prefix matrix suffix)))

(defn pad-x [matrix pad-l pad-r]
  (let [prefix (take pad-l (repeat nil))
        suffix (take pad-r (repeat nil))]
    (map #(concat prefix % suffix) matrix)))

(defn pad-around
  "Adds nil padding around the matrix. Expects a normalized matrix."
  ([matrix] matrix)
  ([matrix pad] (pad-around matrix pad pad pad pad))
  ([matrix pad-y pad-x] (pad-around matrix pad-y pad-x pad-y pad-x))
  ([matrix pad-t pad-r pad-b pad-l]
   (-> matrix
       (pad-y pad-t pad-b)
       (pad-x pad-l pad-r))))

(defn trim-y [matrix trim-t trim-b]
  (->> matrix
       (drop trim-t)
       (take (- (count matrix) trim-t trim-b))))

(defn trim-x [matrix trim-l trim-r]
  (map #(trim-y % trim-l trim-r) matrix))

(defn trim-around
  "Removes outside rows/cols. Expects a normalized matrix."
  ([matrix] matrix)
  ([matrix trim] (trim-around matrix trim trim trim trim))
  ([matrix trim-y trim-x] (trim-around matrix trim-y trim-x trim-y trim-x))
  ([matrix trim-t trim-r trim-b trim-l]
   (-> matrix
       (trim-y trim-t trim-b)
       (trim-x trim-l trim-r))))

(comment
 (parse-file "./known-invaders/1.txt")
 (parse-file "./known-invaders/2.txt")
 (parse-file "./radar-scans/01.txt")

 (pad-y (parse-file "./known-invaders/1.txt") 2 3)
 (pad-x (parse-file "./known-invaders/1.txt") 2 3)
 (pad-around (parse-file "./known-invaders/1.txt") 2)
 (trim-around (parse-file "./known-invaders/1.txt") 2 1))
