(ns types.core-test
  (:require [clojure.test :refer :all]
            [types.core :refer :all]))

(deftest shift-test
  (testing "shifting"
    (is (= (shift {:type :var, :value 0} 1) {:type :var, :value 1}))))
