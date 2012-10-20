(ns ^{:doc "Simple mocking and stubbing for Clojure unit-tests. Supports Clojure 1.2 through 1.5."}
  conjure.core
  (:use clojure.test
        [clojure.string :only [join]]))

(def ^{:doc "Atom holding a map of faked function names to the arglist that they
             were called with. This is used internally by Conjure."}
  call-times (atom {}))

(defn- return-value-fn? [return-value]
  (or (fn? return-value)
      (instance? clojure.lang.MultiFn return-value)))

(defn- apply-return-value-for-fn [return-value args]
  (if (return-value-fn? return-value)
    (apply return-value args)
    return-value))

(defn stub-fn
  "Wraps a function, instrumenting it to record information about when it was
  called. This is used internally by Conjure."
  [function-name return-value]
  (swap! call-times assoc function-name [])
  (fn this [& args]
    (let [args (vec args)] ;; vectors look nicer when reporting
      (swap! call-times update-in [this] conj args)
      (apply-return-value-for-fn return-value args))))

(defn stub-fn-with-return-vals
  "Creates a anonymous function that, on each successive call, returns the next
   item in the supplied return values. When it runs out of values to return, will
   continually return the rightmost supplied return value. Meant to be used on
   the righthand side of a stubbing macro's binding."
  [& return-vals]
  (when (empty? return-vals)
    (throw (RuntimeException. "Looks like you may have forgotten to specify return values.")))
  (let [return-val-atom (atom return-vals)]
    (fn [& _]
      (let [current-result (first @return-val-atom)]
        (when (> (count @return-val-atom) 1)
          (swap! return-val-atom rest))
        current-result))))

(defn mock-fn
  "Wraps a function, instrumenting it to record information about when it was
  called. This is used internally by Conjure."
  [function-name]
  (stub-fn function-name nil))

(defmacro verify-call-times-for
  "Asserts that the faked function was called n times"
  [fn-name n]
  `(is (= ~n (count (get @call-times ~fn-name)))
       (str "(verify-call-times-for " ~fn-name " " ~n ")")))

(defmacro verify-first-call-args-for
  "Asserts that the faked function was called at least once, and the first call
   was passed the args specified"
  [fn-name & args]
  `(do
     (is (= true (pos? (count (get @call-times ~fn-name))))
         (str "(verify-first-call-args-for " ~fn-name " " ~(join " " args) ")"))
     (is (= ~(vec args) (first (get @call-times ~fn-name)))
         (str "(verify-first-call-args-for " ~fn-name " " ~(join " " args) ")"))))

(defmacro verify-called-once-with-args
  "Asserts that the faked function was called exactly once, and was passed the
   args specified"
  [fn-name & args]
  `(do
    (conjure.core/verify-call-times-for ~fn-name 1)
    (conjure.core/verify-first-call-args-for ~fn-name ~@args)))

(defmacro verify-nth-call-args-for
  "Asserts that the function was called n times, and the nth time was passed the
   args specified"
  [n fn-name & args]
  `(is (= ~(vec args) (nth (get @call-times ~fn-name) ~(dec n)))
       (str "(verify-nth-call-args-for " ~n " " ~fn-name " " ~(join " " args) ")")))

(defmacro verify-first-call-args-for-indices
  "Asserts that the function was called at least once, and the first call was
   passed the args specified, into the indices of the arglist specified. In
   other words, it checks only the particular args you care about."
  [fn-name indices & args]
  `(do
     (is (= true (pos? (count (get @call-times ~fn-name))))
         (str "(verify-first-call-args-for-indices " ~fn-name " " ~indices " " ~(join " " args) ")"))
     (let [first-call-args# (first (get @call-times ~fn-name))
           indices-in-range?# (< (apply max ~indices) (count first-call-args#))]
       (if indices-in-range?#
         (is (= ~(vec args) (map #(nth first-call-args# %) ~indices))
             (str "(verify-first-call-args-for-indices " ~fn-name " " ~indices " " ~(join " " args) ")"))
         (is (= :fail (format "indices %s are out of range for the args, %s" ~indices ~(vec args)))
             (str "(verify-first-call-args-for-indices " ~fn-name " " ~indices " " ~(join " " args) ")"))))))

(defmacro mocking
  "Within the body of this macro you may use the various conjure verify-* macros
   to make assertions about how the mocked functions have been called."
  [fn-names & body]
  (let [binding-or-with-redefs (if (= 2 (:minor *clojure-version*))
                                 'binding
                                 'with-redefs)
        mocks (for [name fn-names]
                `(conjure.core/mock-fn ~name))]
    `(try
       (~binding-or-with-redefs [~@(interleave fn-names mocks)]
         ~@body)
       (finally
        (reset! call-times {})))))

(defmacro stubbing
  "Within the body of this macro you may use the various conjure verify-* macros
   to make assertions about how the mocked functions have been called.  This is
   like mocking, except you also specify the return value of the faked functions.
   If the return value is specified as a function, then that function will be
   used as the stub (it won't return the function - instead the function will be
   used in pace of the orginal."
  [stub-forms & body]
  (let [binding-or-with-redefs (if (= 2 (:minor *clojure-version*))
                                 'binding
                                 'with-redefs)
        stub-pairs (partition 2 stub-forms)
        fn-names (map first stub-pairs)
        stubs (for [[fn-name return-value] stub-pairs]
                `(conjure.core/stub-fn ~fn-name ~return-value))]
    `(try
       (~binding-or-with-redefs [~@(interleave fn-names stubs)]
         ~@body)
       (finally
        (reset! call-times {})))))