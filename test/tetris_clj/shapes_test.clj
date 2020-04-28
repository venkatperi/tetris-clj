(ns tetris-clj.shapes-test
  (:require [clojure.test :refer :all]
            [tetris-clj.shapes :as s]))

(deftest matrix-height
  (let [mat [[1 0 0] [0 1 0] [0 0 1] [1 1 1]]]
    (is (= 4 (s/height mat)))))

(deftest matrix-width
  (let [mat [[1 0 0] [0 1 0] [0 0 1] [1 1 1]]]
    (is (= 3 (s/width mat)))))

(deftest every2d-test
  (let [mat [[1 0 0] [0 1 0] [0 0 1] [1 1 1]]]
    (is (= true (s/every2d? #(>= % 0) mat)))
    (is (= false (s/every2d? #(> % 0) mat))))
  (let [mat [[1 1 1] [1 1 1] [1 1 1] [1 1 1]]]
    (is (= true (s/every2d? #(>= % 0) mat)))
    (is (= true (s/every2d? #(= % 1) mat))))
  )

(deftest zero-mat-test
  (is (= true (s/every2d? #(= % 0) (s/zero-mat 2 2))))
  (is (= true (s/every2d? #(= % 0) (s/zero-mat 1 1))))
  (is (= true (s/every2d? #(= % 0) (s/zero-mat 1000 2000)))))

(deftest grow-mat-test
  (let [mat (s/grow-mat [[1 2] [3 4]] 1 1 5 3)]
    (is (= mat [[0 0 0 0 0] [0 1 2 0 0] [0 3 4 0 0]]))
    ))

