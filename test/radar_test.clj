(ns radar-test
  (:require [radar]
            [clojure.test :refer [deftest is testing]]))

(deftest render-pixel-test
  (testing "renders true as 'o'"
    (is (= "o" (radar/render-pixel true))))
  (testing "renders false as '-'"
    (is (= "-" (radar/render-pixel false)))
    (is (= "-" (radar/render-pixel nil)))))

(deftest render-row-test
  (testing "renders a row of pixels"
    (is (= "o-o" (radar/render-row [true false true]))))
  (testing "renders empty row"
    (is (= "" (radar/render-row [])))))

(deftest place-bitmap-in-matrix-test
  (testing "places bitmap at origin"
    (let [matrix [[false false]
                  [false false]]
          bitmap [[true]]]
      (is (= [[true false]
              [false false]]
             (radar/place-bitmap-in-matrix matrix 0 0 bitmap)))))
  (testing "places bitmap at offset"
    (let [matrix [[false false false]
                  [false false false]
                  [false false false]]
          bitmap [[true true]
                  [true false]]]
      (is (= [[false false false]
              [false true true]
              [false true false]]
             (radar/place-bitmap-in-matrix matrix 1 1 bitmap)))))
  (testing "only-white option writes only white pixels"
    (let [matrix [[true false]
                  [false false]]
          bitmap [[false]]]
      (is (= [[true false]
              [false false]]
             (radar/place-bitmap-in-matrix matrix 0 0 bitmap :only-white true)))
      (is (= [[false false]
              [false false]]
             (radar/place-bitmap-in-matrix matrix 0 0 bitmap :only-white false)))))
  (testing "handles nil bitmap pixels"
    (let [matrix [[false]]
          bitmap [[nil true]]]
      (is (= [[false]] (radar/place-bitmap-in-matrix matrix 0 0 bitmap))))))

(deftest add-noise-test
  (testing "adds noise with default value"
    (let [matrix [[true true]
                  [false false]]
          result (radar/add-noise matrix 1.0 "X")]
      (is (= [["X" "X"]
              ["X" "X"]]
             result))))
  (testing "adds noise with custom probability"
    (let [matrix [[true]]
          result (radar/add-noise matrix 0.0)]
      (is (= [[true]] result))))
  (testing "add-noise arities"
    (let [matrix [[false]]]
      (is (= [[false]] (radar/add-noise matrix)))
      (is (= [[false]] (radar/add-noise matrix 0.0)))
      (is (= [[false]] (radar/add-noise matrix 0.0 true))))))

(deftest place-invaders-test
  (testing "places invaders in matrix"
    (let [matrix [[false false]
                  [false false]]
          invaders [[[true]]]
          result (radar/place-invaders matrix invaders)]
      (is (= 2 (count result)))
      (is (= 2 (count (first result))))))
  (testing "places multiple invaders"
    (let [matrix [[false false]
                  [false false]]
          invaders [[[true]]]
          result (radar/place-invaders matrix invaders)]
      (is (= 2 (count result)))
      (is (= 2 (count (first result)))))))

(deftest scan-test
  (testing "creates scan with default invaders"
    (let [result (radar/scan 5 5 nil)]
      (is (= 5 (count result)))
      (is (= 5 (count (first result))))))
  (testing "creates scan with custom invaders"
    (let [invaders #{[[true false]
                      [false true]]}
          result (radar/scan 10 10 invaders)]
      (is (= 10 (count result)))
      (is (= 10 (count (first result))))))
  (testing "scan handles empty invaders"
    (let [result (radar/scan 3 3 #{[[]]})]
      (is (= 3 (count result)))
      (is (= 3 (count (first result)))))))
