(ns dartgen.core-test
  (:require [clojure.test :refer :all]
            [dartgen.core :refer :all]))

(deftest darts
  (let [board nil]
    (is (= 1 (low-score-unattainable 0 board)))
    (is (= 1 (low-score-unattainable -1 board)))
    (is (= 1 (low-score-unattainable 10 board))))
  (let [board [2 3 4]]
    (is (= 1 (low-score-unattainable 0 board)))
    (is (= 1 (low-score-unattainable 1 board)))
    (is (= 1 (low-score-unattainable 10 board))))
  (let [board [1 2 3 4]]
    (is (= 1 (low-score-unattainable 0 board)))
    (is (= 5 (low-score-unattainable 1 board)))
    (is (= 9 (low-score-unattainable 2 board))))
  (let [board [1 3]]
    (is (= 1 (low-score-unattainable 0 board)))
    (is (= 2 (low-score-unattainable 1 board)))
    (is (= 5 (low-score-unattainable 2 board)))
    (is (= 8 (low-score-unattainable 3 board))))
  (let [board [1 3 7]]
    (is (= 1 (low-score-unattainable 0 board)))
    (is (= 2 (low-score-unattainable 1 board)))
    (is (= 5 (low-score-unattainable 2 board)))
    (is (= 12 (low-score-unattainable 3 board))))
  (let [board [1 4 7]]
    (is (= 1 (low-score-unattainable 0 board)))
    (is (= 2 (low-score-unattainable 1 board)))
    (is (= 3 (low-score-unattainable 2 board)))
    (is (= 10 (low-score-unattainable 3 board))))
  (let [board [1 2 5 6]]
    (is (= 1 (low-score-unattainable 0 board)))
    (is (= 3 (low-score-unattainable 1 board)))
    (is (= 9 (low-score-unattainable 2 board)))
    (is (= 19 (low-score-unattainable 3 board)))))



