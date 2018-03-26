(ns dartgen.core-test
  (:require [clojure.test :refer :all]
            [dartgen.core :refer :all]))

(deftest darts
  (let [board nil]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 1 (no-can-dart -1 board)))
    (is (= 1 (no-can-dart 10 board))))
  (let [board [2 3 4]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 1 (no-can-dart 1 board)))
    (is (= 1 (no-can-dart 10 board))))
  (let [board [1 2 3 4]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 5 (no-can-dart 1 board)))
    (is (= 9 (no-can-dart 2 board))))
  (let [board [1 3]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 2 (no-can-dart 1 board)))
    (is (= 5 (no-can-dart 2 board)))
    (is (= 8 (no-can-dart 3 board))))
  (let [board [1 3 7]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 2 (no-can-dart 1 board)))
    (is (= 5 (no-can-dart 2 board)))
    (is (= 12 (no-can-dart 3 board))))
  (let [board [1 4 7]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 2 (no-can-dart 1 board)))
    (is (= 3 (no-can-dart 2 board)))
    (is (= 10 (no-can-dart 3 board))))
  (let [board [1 2 5 6]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 3 (no-can-dart 1 board)))
    (is (= 9 (no-can-dart 2 board)))
    (is (= 15 (no-can-dart 3 board)))))
