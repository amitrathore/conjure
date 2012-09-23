(ns conjure.core
  (:use clojure.test))

(def call-times (atom {}))

(defn is-return-value-fn? [return-value]
  (or (fn? return-value)
      (instance? clojure.lang.MultiFn return-value)))

(defn apply-return-value-for-fn [return-value args]
  (if (is-return-value-fn? return-value)
    (apply return-value args)
    return-value))

(defn stub-fn [function-name return-value]
  (swap! call-times assoc function-name [])
  (fn this [& args]
    (swap! call-times update-in [function-name] conj args)
    (swap! call-times update-in [this] conj args)
    (apply-return-value-for-fn return-value args)))

(defn stub-fn-with-return-vals [function-name return-values]
  (swap! call-times assoc function-name [])
  (fn this [& args]
    (swap! call-times update-in [function-name] conj args)
    (let [call-times-for-fn (count (@call-times function-name))]
      (if (< call-times-for-fn (count return-values))
        (apply-return-value-for-fn (nth return-values (dec call-times-for-fn)) args)
        (apply-return-value-for-fn (last return-values) args)))))

(defn mock-fn [function-name]
  (stub-fn function-name nil))

(defn verify-call-times-for [fn-name number]
  (is (= number (count (@call-times fn-name)))))

(defn verify-first-call-args-for [fn-name & args]
  (is (= args (first (@call-times fn-name)))))

(defn verify-called-once-with-args [fn-name & args]
  (verify-call-times-for fn-name 1)
  (apply verify-first-call-args-for fn-name args))

(defn verify-nth-call-args-for [n fn-name & args]
  (is (= args (nth (@call-times fn-name) (dec n)))))

(defn verify-first-call-args-for-indices [fn-name indices & args]
  (let [first-call-args (first (@call-times fn-name))
        indices-in-range? (< (apply max indices) (count first-call-args))]
    (if indices-in-range?
      (is (= args
             (map #(nth first-call-args %) indices)))
      (is (= :fail (format "indices %s are out of range for the args, %s" indices args))))))

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

(def binding-or-with-redefs (if (= 2 (:minor *clojure-version*))
                              'binding
                              'with-redefs))

(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        fn-names (map first stub-pairs)
        stubs (for [[fn-name return-value] stub-pairs]
               `(conjure.core/stub-fn ~fn-name ~return-value))]
    `(~binding-or-with-redefs [~@(interleave fn-names stubs)]
       ~@body)))

(defmacro stubbing-with-return-vals [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        fn-names (map first stub-pairs)
        stubs (for [[fn-name return-value] stub-pairs]
               `(conjure.core/stub-fn-with-return-vals ~fn-name ~return-value))]
    `(~binding-or-with-redefs [~@(interleave fn-names stubs)]
       ~@body)))