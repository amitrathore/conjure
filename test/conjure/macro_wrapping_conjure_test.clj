(ns conjure.macro_wrapping-conjure-test
  (:use conjure.ns-with-macro))

(defn foo []
  (with-nil?-always-foo
    :if-this-code-compiles-we-can-successfully-create-a-macro))

(defn bar []
  (with-mocked-nil?
    :if-this-code-compiles-we-can-successfully-create-a-macro))



