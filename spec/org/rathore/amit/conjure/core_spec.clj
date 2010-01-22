(ns conjure-spec
  (:use [clojure.test :only [run-tests deftest is are]])
  (:use org.rathore.amit.conjure.core))

(defn xx [a b]
  10)

(defn yy [z]
  20)

(defn fn-under-test []
  (xx 1 2)
  (yy "blah"))

(defn another-fn-under-test []
  (+ (xx nil nil) (yy nil)))

(deftest test-basic-mocking
  (mocking [xx yy]
    (fn-under-test))
  (verify-call-times-for xx 1)
  (verify-call-times-for yy 1)
  (verify-first-call-args-for xx 1 2)
  (verify-first-call-args-for yy "blah"))

(deftest test-basic-stubbing
  (is (= (another-fn-under-test) 30))
  (stubbing [xx 1 yy 2]
    (is (= (another-fn-under-test) 3))))