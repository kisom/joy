(ns joy.quicksort)

(defn nom [n]
  (take n
	(repeatedly #(rand-int n))))

(defn sort-parts
  [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)]
	 (recur (list* (filter smaller? xs)
		       pivot
		       (remove smaller? xs)
		       parts)))
       (when-let [[x & parts] parts]
	 (cons x (sort-parts parts)))))))

(defn quick-sort [xs]
  (sort-parts (list xs)))
