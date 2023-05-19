(ns covid19-spread-simulation.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [covid19-spread-simulation.core :as core]))

(deftest fake-test
  (testing "fake description"
    (is (= 1 (* 1 1e0) 1.00 1M))))

(deftest irrelevant-test
  (testing "playing around"
    (is (= 10 (* 2 5 -1 -1)))))
