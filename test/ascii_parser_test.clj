(ns ascii-parser-test
  (:require [ascii-parser]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest normalize-test
  (testing "returns original matrix when rows are even"
    (is (= [[1 2] [3 4]]
           (ascii-parser/normalize [[1 2] [3 4]]))))
  (testing "pads shorter rows with nil to max width"
    (is (= [[true false nil]
            [false true true]
            [nil nil nil]]
           (ascii-parser/normalize [[true false]
                           [false true true]
                           []])))))

(deftest parse-line-test
  (is (= [false true nil nil]
         (vec (ascii-parser/parse-line "-o x")))))

(deftest parse-input-test
  (is (= [[false true]
          [true nil]]
         (mapv vec (ascii-parser/parse-input ["-o" "o"])))))

(deftest parse-file-test
  (let [tmp (java.io.File/createTempFile "ascii-parser" ".txt")]
    (try
      (spit tmp "-o\no\n")
      (is (= [[false true]
              [true nil]]
             (mapv vec (ascii-parser/parse-file (.getAbsolutePath tmp)))))
      (finally
        (io/delete-file tmp true)))))

(deftest pad-around-test
  (let [m [[true false]
           [false true]]]
    (testing "single-pad arity pads all sides equally"
      (is (= [[nil nil nil nil]
              [nil true false nil]
              [nil false true nil]
              [nil nil nil nil]]
             (mapv vec (ascii-parser/pad-around m 1)))))
    (testing "4-arg arity pads each side independently"
      (is (= [[nil nil nil nil]
              [nil true false nil]
              [nil false true nil]
              [nil nil nil nil]
              [nil nil nil nil]]
             (mapv vec (ascii-parser/pad-around m 1 1 2 1)))))))

(deftest trim-around-test
  (let [m [[nil nil nil nil]
           [nil true false nil]
           [nil false true nil]
           [nil nil nil nil]
           [nil nil nil nil]]]
    (testing "single-trim arity trims all sides equally"
      (is (= [[true false]
              [false true]
              [nil nil]]
             (mapv vec (ascii-parser/trim-around m 1)))))
    (testing "4-arg arity trims each side independently"
      (is (= [[true false]
              [false true]]
             (mapv vec (ascii-parser/trim-around m 1 1 2 1)))))))

