(ns conjure.core-test
  (:use [clojure.test :only [run-tests deftest is are]]
        conjure.core))

(defn xx [a b]
  10)

(defn yy [z]
  20)

(defn fn-under-test []
  (xx 1 2)
  (yy "blah"))

(defn another-fn-under-test [& _args]
  (+ (xx nil nil)
     (yy nil)))

(deftest test-basic-mocking
  (mocking [xx yy]
    (fn-under-test))
  (verify-call-times-for xx 1)
  (verify-call-times-for yy 1)
  (verify-first-call-args-for xx 1 2)
  (verify-first-call-args-for yy "blah")

  ;; common case: combines fact that it had one call and its args were correct
  (verify-called-once-with-args xx 1 2)
  (verify-called-once-with-args yy "blah"))


(deftest test-verifying-only-one-call-and-its-args
  (mocking [xx yy]
    (fn-under-test))
  (verify-call-times-for xx 1)
  (verify-call-times-for yy 1)
  (verify-first-call-args-for xx 1 2)
  (verify-first-call-args-for yy "blah"))

(clear-calls)

(deftest test-mocking-can-verify-within-mocking-scope
  (mocking [xx yy]
    (fn-under-test)
    (verify-call-times-for xx 1)
    (verify-call-times-for yy 1)
    (verify-first-call-args-for xx 1 2)
    (verify-first-call-args-for yy "blah")))

(deftest test-basic-stubbing
  (is (= (another-fn-under-test) 30))
  (stubbing [xx 1 yy 2]
    (is (= (another-fn-under-test) 3))))

(deftest test-fn-based-stubs
  (is (= (another-fn-under-test) 30))
  (stubbing [xx 1 yy (fn [_] (+ 2 3))]
    (is (= (another-fn-under-test) 6)))
  (stubbing [xx 1 yy (fn [_] (+ 2 (xx :a :b )))]
    (is (= (another-fn-under-test) 4))))

(defn three-arg-fn [a b c]
  "Bonjour!")

(deftest test-verify-first-call-args-for-indices
  (mocking [three-arg-fn]
           (three-arg-fn "one" "two" "three"))
  (verify-first-call-args-for-indices three-arg-fn [0 2] "one" "three"))

(defn f []
  "called f")

(deftest test-passes-args-through-to-fake-fn
  (stubbing [f (fn [& msgs] msgs)]
    (is (= ["a" "b" "c"] (f "a" "b" "c")))))

(deftest test-can-stub-multimethods
  (defmulti mm :shape)
  (defmethod mm :triangle [x]
    "called triangle multimethod")
  (stubbing [f mm]
    (is (= "called triangle multimethod" (f {:shape :triangle})))))