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
    (are [x y z] (= (subst-top x y) z (subst-top-direct x y))
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

(deftest subst-test
  (testing "substitutes a term"
    (are [x c y z] (= (subst x c y) z)
      (variable 0) 0 (variable 0) (variable 0)
      (variable 0) 1 (variable 1) (variable 0)
      (variable 1) 1 (variable 0) (variable 0)
      (variable 1) 2 (variable 0) (variable 1)
      (variable 2) 2 (variable 1) (variable 1)
      (variable 3) 2 (variable 10) (variable 3)
      (variable 100) 100 (variable 20) (variable 20)
      (abs (variable 0)) 0 (variable 18) (abs (variable 0))
      (abs (variable 1)) 0 (variable 18) (abs (variable 19))
      (abs (variable 1)) 1 (variable 18) (abs (variable 1))
      (app (variable 3) (abs (variable 9))) 8 (abs (variable 35)) (app (variable 3) (abs (abs (variable 36)))))))

(deftest eval-n-test
  (testing "evaluates in the normal order"
    (let [v0 (variable 0)
          v4 (variable 4)
          v5 (variable 5)]
      (are [x] (= (eval-n x) x)
        v0
        v4
        v5
        (abs v0)
        (abs v4)
        (abs v5)
        (app v0 v5)
        (app v5 (abs v0))
        (app (app v0 v0) v5)
        (app v5 (app v5 v5)))
      (are [x y] (= (eval-n x) y)
        (app (abs v0) v5) v5
        (app (abs v5) v0) v4
        (abs (app (abs v0) v5)) (abs v5)
        (abs (app v0 (app (abs v5) (abs v0)))) (abs (app v0 v4))))))

(deftest skk
  (testing "skk is i"
    (let [skk (app s-combinator (app k-combinator k-combinator))])
    (are [x] (= (app  x) (app i-combinator x))
      (let [v0 (variable 0)]
        v0))))
