(ns joy.persistent)

(def baselist (list :hollis :milgrim))
(def lst1 (cons :gareth baselist))
(def lst2 (cons :bigend baselist))

(defn xconj [t v]
  (cond
   (nil? t)        {:val v, :L nil :R nil}
   (< v (:val t))  {:val (:val t)
		    :L (xconj (:L t) v)
		    :R (:R t)}
   :else           {:val (:val t)
		    :L (:L t)
		    :R (xconj (:R t) v)}))

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)))))

(defn lz-rec-step [s]
  (lazy-seq
   (if (seq s)
     [(first s) (lz-rec-step (rest s))]
     [])))

(defn simple-range [i limit]
  (lazy-seq
   (when (< i limit)
     (cons i (simple-range (inc i) limit)))))

(defn map-reduce [mf rf coll]
  (reduce rf (map mf coll)))

(defn filter-reduce [ff rf coll]
  (reduce rf (filter ff coll)))

(defn triangle [n]
  (/
   (* n (+ n 1))
   2))

(defn inf-triangles [n]
  {:head (triangle n)
   :tail (delay (inf-triangles (inc n)))})

(defn head [l]
  (:head l))

(defn tail [l]
  (force (:tail l)))

(def tri-nums (inf-triangles 1))

(defn taker [n l]
  (loop [t n, src l, ret []]
    (if (zero? t)
      ret
      (recur (dec t) (tail src) (conj ret (head src))))))

(defn nthr [l n]
  (if (zero? n)
    (head l)
    (recur (tail l) (dec n))))
