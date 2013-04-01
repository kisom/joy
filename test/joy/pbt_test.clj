(ns joy.pbt-test
  (:use clojure.test
        joy.pbt)
  (:import joy.pbt.PBT))

(deftest roots-identical
  (testing "Roots are equal"
    (let [baselist (xconj nil 5)
          list1 (xconj baselist 3)
          list2 (conj baselist 7)]
      (is (identical? (:l list1) (:r list2))))))
