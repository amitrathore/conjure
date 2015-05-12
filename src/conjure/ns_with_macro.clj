(ns conjure.ns-with-macro
  (:use conjure.core))

(defmacro with-nil?-always-foo [& body]
  `(stubbing [nil? :foo]
             ~@body))

(defmacro with-mocked-nil? [& body]
  `(mocking [nil?]
            ~@body))
