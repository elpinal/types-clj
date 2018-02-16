(ns types.core-test
  (:require [clojure.test :refer :all]
            [types.core :refer :all]))

(deftest shift-test
  (testing "shifting"
    (is (= (shift (variable 0) 1) (variable 1)))))
