(ns joy.func)
(def plays [{:band "Radiohead" :plays 85 :loved 13}
	    {:band "Zeromancer" :plays 73 :loved 27}
	    {:band "Segment" :plays 71 :loved 15}])

(defn columns [names]
  (fn [row]
    (vec (map row names))))

(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))

(defn manip-map [f ks m]
  (conj m (keys-apply f ks m)))

(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks) plays))

(defn slope [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
	    (- (p2 0) (p1 0)))))

(defn slope-constrained [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(def bearings
  [{:x 0 :y 1}    ; north
   {:x 1 :y 0}    ; east
   {:x 0 :y -1}   ; south
   {:x -1 :y 0}]) ; west

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
			(+ y (:y (bearings bearing-num)))
			bearing-num))})

(defn bot [x y bearing-num]
  {:coords     [x y]
   :bearing    ([:north :east :south :west] bearing-num)
   :forward    (fn [] (bot (+ x (:x (bearings bearing-num)))
			   (+ y (:y (bearings bearing-num)))
			   bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left  (fn [] (bot x y (mod (- 1 bearing-num) 4)))})


(defn pow [x y]
  (if (zero? y)
    1
    (* x (pow x (dec y)))))

(defn pow-tco [x y]
  (letfn [(_pow [base exp acc]
	    (if (zero? exp)
	      acc
	      (recur base (dec exp) (* base acc))))]
    (_pow x y 1)))

(defn elevator [commands]
  (letfn
      [(ff-open [[cmd & r]]
	 #(case cmd
	    :close (ff-closed r)
	    :done true
	    false))
       (ff-closed [[cmd & r]]
	 #(case cmd
	    :open (ff-open r)
	    :up (sf-closed r)
	    false))
       (sf-closed [[cmd & r]]
	 #(case cmd
	    :down (ff-closed r)
	    :open (sf-open r)
	    false))
       (sf-open [[cmd & r]]
	 #(case cmd
	    :close (sf-closed r)
	    :done true
	    false))]
    (trampoline ff-open commands)))

(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

;; accept is a function that decides when the computation must
;; terminate.
(defn mk-cps [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v] (k (kont v n)))]
	 (if (accept? n)
	   (k end-value)
	   (recur (dec n) cont))))
     n kend)))
