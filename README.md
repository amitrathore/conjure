Conjure
=======

Simple mocking and stubbing for Clojure unit-tests. Supports Clojure 1.2 through 1.5.

Usage
=====

```clj
[org.clojars.runa/conjure "2.1.2"]

(:require [conjure.core])
```

The Set up
==========

Imagine we had the following functions -

```clj
(defn xx [a b]
  10)

(defn yy [z]
  20)

(defn fn-under-test []
  (xx 1 2)
  (yy  "blah"))

(defn another-fn-under-test []
  (+ (xx nil nil) (yy nil)))
```

Also imagine that we had to test fn-under-test and another-fn-under-test, and we didn’t want to have to deal with the xx or yy functions. Maybe they’re horrible functions that open connections to computers running Windoze or something, I dunno.

Mocking
=======

Here’s how we might mock them out -

```clj
(deftest test-basic-mocking
  (mocking [xx yy]
    (fn-under-test)
    (verify-call-times-for xx 1)
    (verify-call-times-for yy 1)
    (verify-first-call-args-for xx 1 2)
    (verify-first-call-args-for yy "blah")))
```

Pretty straightforward, eh? You just use the `mocking` macro, specifying all the functions that need to be mocked out. Then, within the scope of `mocking`, you call your functions that need to be tested. The calls to the specified functions will get mocked out (they won’t occur), and you can then use things like verify-call-times-for and verify-first-call-args-for to ensure things worked as expected.

Stubbing
========

Sometimes your tests need to specify values to be returned by the functions being mocked out. That’s where `stubbing` comes in. 

Here’s how it works -

```clj
(deftest test-basic-stubbing
  (is (= (another-fn-under-test) 30))
  (stubbing [xx 1 yy 2]
    (is (= (another-fn-under-test) 3))))
```

So that’s it! Pretty simple. Note how within the scope of `stubbing`, `xx` returns `1` and `yy` returns `2`. Now, for the implementation.

Instrumenting
=============

Sometimes you just want to inspect the calls of some function without otherwise interfering with its execution. 

```clj
(defn my-inc [n]
  (inc n)) ;; inc has :inline metadata so cannot be faked

(deftest test-instumenting
  (instrumenting [my-inc]
                 (is (= 43 (my-inc 42)))
                 (verify-called-once-with-args my-inc 42)))
```

