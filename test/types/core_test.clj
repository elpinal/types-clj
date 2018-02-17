(ns types.core-test
  (:require [clojure.test :refer :all]
            [types.core :refer :all]))

(deftest shift-test
  (testing "shifting"
    (are [x n y] (= (shift x n) y)
      (variable 0) 1 (variable 1)
      (abs (variable 0)) 3 (abs (variable 0))
      (abs (variable 1)) 3 (abs (variable 4))
      (app (variable 1) (variable 2)) 8 (app (variable 9) (variable 10))
      (app (abs (variable 0)) (variable 2)) 8 (app (abs (variable 0)) (variable 10))
      (variable 0) 0 (variable 0)
      (variable 0) -1 (variable -1)
      (variable 1) -1 (variable 0))))

(deftest subst-top-test
  (testing "substitutes a term"
    (are [x y z] (= (subst-top x y) z)
      (variable 0) (variable 0) (variable 0)
      (variable 0) (variable 1) (variable 1)
      (variable 0) (variable 10) (variable 10)
      (variable 1) (variable 0) (variable 0)
      (variable 1) (variable 1) (variable 0)
      (variable 1) (variable 10) (variable 0)
      (variable 100) (variable 20) (variable 99)
      (abs (variable 0)) (variable 18) (abs (variable 0))
      (abs (variable 1)) (variable 18) (abs (variable 19))
      (app (variable 3) (abs (variable 9))) (abs (variable 35)) (app (variable 2) (abs (variable 8))))))
