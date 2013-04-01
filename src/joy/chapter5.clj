(ns joy.chapter5)

(def test-vec [\h \e \l \l \o \, \space \w \o \r \l \d])
(def matrix
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(def large-matrix
  [[ 1  2  3  4  5]
   [ 6  7  8  9 10]
   [11 12 13 14 15]
   [16 17 18 19 20]
   [21 22 23 24 25]])

(defn- neighbours
  ([size yx] (neighbours [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
     (filter (fn [new-yx]
	       (every? #(< -1 % size) new-yx))
	     (map #(map + yx %) deltas))))

(defn matrix-neighbours
  [matrix coords]
  (let [[x y] coords
	bound (count (first matrix))]
    (cond (or (neg? x) (neg? y)) :bad-coordinates
	  (or (>= x bound) (>= y bound)) :bad-coordinates
	  :else
	  (map #(get-in matrix %)
	       (neighbours bound coords)))))

(def alpha-set
  (set
   (map #(keyword (str %))
	(map char (range 97 123)))))

(def alpha-vec
  (apply vector
   (map #(keyword (str %))
	(map char (range 97 123)))))


(defn index [coll]
  (cond
   (map? coll) (seq coll)
   (set? coll) (map vector coll coll)
   :else (map vector (iterate inc 0) coll)))

(defn pos [pred coll]
  (for [[i v] (index coll) :when (pred v)] i))
