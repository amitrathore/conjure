(ns conjure.core
  (:use clojure.test))

(def call-times (atom {}))

(defn stub-fn [function-name return-value]
  (swap! call-times assoc function-name [])
  (fn [& args]
    (swap! call-times update-in [function-name] conj args)
    (if (fn? return-value)
      (apply return-value args)
      return-value)))

(defn mock-fn [function-name]
  (stub-fn function-name nil))

(defn verify-call-times-for [fn-name number]
  (is (= number (count (@call-times fn-name)))))

(defn verify-first-call-args-for [fn-name & args]
  (is (= args (first (@call-times fn-name)))))

(defn verify-nth-call-args-for [n fn-name & args]
  (is (= args (nth (@call-times fn-name) (dec n)))))

(defn clear-calls []
  (reset! call-times {}))

(defmacro mocking [fn-names & body]
  (let [mocks (map #(list 'conjure.core/mock-fn %) fn-names)]
    `(with-redefs [~@(interleave fn-names mocks)]
       ~@body)))

(defmacro stubbing [stub-forms & body]
  (let [stub-pairs (partition 2 stub-forms)
        fn-names (map first stub-pairs)
        stubs (map #(list 'conjure.core/stub-fn (first %) (last %)) stub-pairs)]
    `(with-redefs [~@(interleave fn-names stubs)]
       ~@body)))