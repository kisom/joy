(ns joy.core-test
  (:use clojure.test
        joy.core))

(deftest a-test
  (testing "Is this the real life?"
    (is (= :this :the-real-life))))
