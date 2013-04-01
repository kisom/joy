(ns joy.chapter4)

(def population {:zombies 2700 :humans 9})

(defn pour [lb ub]
  (cond (= ub :toujours) (iterate inc lb)
        :else (range lb ub)))
