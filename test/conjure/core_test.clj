(ns conjure.core-test
  (:use [clojure.test :only [run-tests deftest is are]]
        conjure.core))


(deftest test-all-public-api-fns-have-docstrings
  (is (= []
         (->> (ns-publics 'conjure.core)
              vals
              (remove (comp :doc meta))))))

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
           (verify-call-times-for xx 0)
           (verify-call-times-for yy 0)
           (fn-under-test)
           (verify-call-times-for xx 1)
           (verify-call-times-for yy 1)
           (verify-first-call-args-for xx 1 2)
           (verify-first-call-args-for yy "blah")

           ;; common case: combines fact that it had one call and its args were correct
           (verify-called-once-with-args xx 1 2)
           (verify-called-once-with-args yy "blah")))

(deftest test-basic-stubbing
  (is (= (another-fn-under-test) 30))
  (stubbing [xx 1 yy 2]
            (is (= (another-fn-under-test) 3))
            (verify-called-once-with-args xx nil nil)
            (verify-called-once-with-args yy nil)))

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
           (three-arg-fn "one" "two" "three")
           (verify-first-call-args-for-indices three-arg-fn [0 2] "one" "three")))

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

(defn a [] "a")

(deftest test-stub-fn-with-return-vals
  (stubbing [a (stub-fn-with-return-vals "b" "c" "d")]
            (is (= "b" (a)))
            (is (= "c" (a)))
            (is (= "d" (a)))
            (is (= "d" (a)))
            (is (= "d" (a))))

  (is (thrown-with-msg?
        RuntimeException
        #"Looks like you may have forgotten to specify return values"
        (stub-fn-with-return-vals))))

(defn my-inc [n]
  (inc n)) ;; inc has :inline metadata so cannot be faked

(deftest test-instumenting
  (instrumenting [my-inc]
                 (is (= 43 (my-inc 42)))
                 (verify-called-once-with-args my-inc 42)))

(deftest test-verifies-only-called-from-within-fake-contexts
  (is (thrown-with-msg?
        AssertionError
        #"cannot be called outside"
        (verify-call-times-for my-inc 2)))

  (is (thrown-with-msg?
        AssertionError
        #"cannot be called outside"
        (verify-first-call-args-for my-inc 2)))

  (is (thrown-with-msg?
        AssertionError
        #"cannot be called outside"
        (verify-called-once-with-args my-inc 2)))

  (is (thrown-with-msg?
        AssertionError
        #"cannot be called outside"
        (verify-nth-call-args-for 0 my-inc 2)))
  
  (is (thrown-with-msg?
        AssertionError
        #"cannot be called outside"
        (verify-first-call-args-for-indices my-inc [0] 2))))

(deftest test-verifies-only-called-on-conjurified-fns
  (mocking []
           (is (thrown-with-msg?
                 AssertionError
                 #"was called on a function that was not specified"
                 (verify-call-times-for my-inc 2))))

  (mocking []
           (is (thrown-with-msg?
                 AssertionError
                 #"was called on a function that was not specified"
                 (verify-first-call-args-for my-inc 2))))

  (mocking []
           (is (thrown-with-msg?
                 AssertionError
                 #"was called on a function that was not specified"
                 (verify-called-once-with-args my-inc 2))))

  (mocking []
           (is (thrown-with-msg?
                 AssertionError
                 #"was called on a function that was not specified"
                 (verify-nth-call-args-for 0 my-inc 2))))

  (mocking []
           (is (thrown-with-msg?
                 AssertionError
                 #"was called on a function that was not specified"
                 (verify-first-call-args-for-indices my-inc [0] 2)))))


(deftest test-dissallows-nesting
  (mocking [inc]
           (is (thrown-with-msg?
                 AssertionError
                 #"cannot be called from within"
                 (stubbing [str "a"]))))
  (stubbing [inc 3]
           (is (thrown-with-msg?
                 AssertionError
                 #"cannot be called from within"
                 (instrumenting [str]))))
  (instrumenting [inc]
           (is (thrown-with-msg?
                 AssertionError
                 #"cannot be called from within"
                 (mocking [str])))))

