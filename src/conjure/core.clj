(ns conjure.core
  (:use clojure.test))

(def call-times (atom {}))

(defn- is-return-value-fn? [return-value]
  (or (fn? return-value)
      (instance? clojure.lang.MultiFn return-value)))

(defn- apply-return-value-for-fn [return-value args]
  (if (is-return-value-fn? return-value)
    (apply return-value args)
    return-value))

(defn stub-fn [function-name return-value]
  (swap! call-times assoc function-name [])
  (fn this [& args]
    (swap! call-times update-in [function-name] conj args)
    (swap! call-times update-in [this] conj args)
    (apply-return-value-for-fn return-value args)))

(defn stub-fn-with-return-vals [return-vals]
  (let [return-val-atom (atom return-vals)]
    (fn [& _]
      (let [current-result (first @return-val-atom)]
        (when (> (count @return-val-atom) 1)
          (swap! return-val-atom rest))
        current-result))))

(defn mock-fn [function-name]
  (stub-fn function-name nil))

(defmacro verify-call-times-for [fn-name number]
  `(is (= ~number (count (get @call-times ~fn-name)))))

(defmacro verify-first-call-args-for [fn-name & args]
  `(do
    (is (= true (pos? (count (get @call-times ~fn-name)))))
    (is (= ~(vec args) (first (get @call-times ~fn-name))))))

(defmacro verify-called-once-with-args [fn-name & args]
  `(do
    (conjure.core/verify-call-times-for ~fn-name 1)
    (conjure.core/verify-first-call-args-for ~fn-name ~@args)))

(defmacro verify-nth-call-args-for [n fn-name & args]
  `(is (= ~(vec args) (nth (get @call-times ~fn-name) ~(dec n)))))

(defmacro verify-first-call-args-for-indices [fn-name indices & args]
  `(do
     (is (= true (pos? (count (get @call-times ~fn-name)))))
     (let [first-call-args# (first (get @call-times ~fn-name))
           indices-in-range?# (< (apply max ~indices) (count first-call-args#))]
       (if indices-in-range?#
         (is (= ~(vec args)
                (map #(nth first-call-args# %) ~indices)))
         (is (= :fail (format "indices %s are out of range for the args, %s" ~indices ~(vec args))))))))

(defn clear-calls []
  (reset! call-times {}))

(defmacro mocking [fn-names & body]
  (let [binding-or-with-redefs (if (= 2 (:minor *clojure-version*))
                                 'binding
                                 'with-redefs)
        mocks (for [name fn-names]
                `(conjure.core/mock-fn ~name))]
    `(~binding-or-with-redefs [~@(interleave fn-names mocks)]
       ~@body)))

(defmacro stubbing [stub-forms & body]
  (let [binding-or-with-redefs (if (= 2 (:minor *clojure-version*))
                                 'binding
                                 'with-redefs)
        stub-pairs (partition 2 stub-forms)
        fn-names (map first stub-pairs)
        stubs (for [[fn-name return-value] stub-pairs]
                `(conjure.core/stub-fn ~fn-name ~return-value))]
    `(~binding-or-with-redefs [~@(interleave fn-names stubs)]
       ~@body)))