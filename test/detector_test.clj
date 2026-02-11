(ns detector-test
  (:require [clojure.test :refer [deftest is testing]]
            [detector]))

(deftest initialize-zero-test
  (is (= [[0 0 0]
          [0 0 0]]
         (mapv vec (detector/initialize-zero [[true false true]
                                              [false true false]])))))

(deftest get-matrix-chunk-test
  (let [m [[1 2 3]
           [4 5 6]
           [7 8 9]]]
    (is (= [[1 2]
            [4 5]]
           (mapv vec (detector/get-matrix-chunk m 1 1 2 2))))
    (is (= [[5 6]
            [8 9]]
           (mapv vec (detector/get-matrix-chunk m 2 2 2 2))))))

(deftest compare-chunks-test
  (testing "perfect match returns exact ratio"
    (is (= 1 (detector/compare-chunks [[true false]] [[true false]]))))
  (testing "partial non-nil match returns exact ratio"
    (is (= 1/2 (detector/compare-chunks [[true false]] [[true true]]))))
  (testing "mismatch scores zero"
    (is (= 0 (detector/compare-chunks [[true true]] [[false false]]))))
  (testing "shape mismatch is caught and returns 0"
    (is (= 0.55 (detector/compare-chunks [[true true]] [[true]])))))

(deftest update-detection-matrix-test
  (is (= [[0 0 0 0]
          [0 1 1 0]
          [0 1 1 0]
          [0 0 0 0]]
         (mapv vec
               (detector/update-detection-matrix [[0 0 0 0]
                                                  [0 0 0 0]
                                                  [0 0 0 0]
                                                  [0 0 0 0]]
                                                 1 1 2 2)))))

(deftest sum-matrices-test
  (is (= [[2 2]
          [3 3]
          [4 4]]
         (mapv vec
               (detector/sum-matrices [[1 1] [2 2] [3 3]]
                                      [[1 1] [1 1] [1 1]])))))

(deftest detect-test
  (let [invader [[true false]
                 [false true]]
        matrix [[true false false]
                [false true false]
                [false false false]]
        detections (atom [])
        result (mapv vec (detector/detect detections matrix invader))]
    (is (= [[0 0 0]
            [0 1 1]
            [0 1 1]]
           result))
    (is (= [[invader 2 2]]
           @detections))))

(deftest detect-edge-range-test
  (let [invader [[true false]
                 [false true]]
        matrix [[false false false]
                [false true false]
                [false false true]]
        detections (atom [])
        result (mapv vec (detector/detect detections matrix invader))]
    (is (= [[0 0 0]
            [0 0 0]
            [0 0 1]]
           result))
    (is (= [[invader 3 3]]
           @detections))))
