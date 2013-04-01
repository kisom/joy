(ns joy.pbt)

;;; persistent binary trees with defrecord

(defrecord PBT [value l r])
(defn xconj [t v]
  (cond
   (nil? t)         (PBT. v nil nil)
   (< v (:value t)) (PBT. (:value t)
                          (xconj (:l t) v)
                          (:r t))
   :else            (PBT. (:value t)
                          (:l t)
                          (xconj (:r t) v))))
