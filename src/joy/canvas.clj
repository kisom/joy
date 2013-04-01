(ns joy.canvas)

(defn xors [max-x max-y]
  (for [x (range max-x) y (range max-y)] [x y (rem (bit-xor x y) 256)]))

(defn setup-frame []
  (doto (java.awt.Frame.)
    (.setVisible true)
    (.setSize (java.awt.Dimension. 200 200))))

;(def root-frame (setup-frame))

;(def gfx (.getGraphics root-frame))

(defn set-gfx-colour [gfx rgb-vec]
  (let [[red green blue] rgb-vec]
    (.setColor gfx (java.awt.Color. red green blue))))

(defn hide-frame [frame]
  (.setVisible frame false))

(defn show-frame [frame]
  (.setVisible frame true))

(defn draw-xor [& args]
  (let [frame (if (empty? args) (setup-frame) (first args))
        gfx (.getGraphics frame)]
    (doseq [[x y xor] (xors 500 500)]
            (set-gfx-colour gfx [xor xor xor])
            (.fillRect gfx x y 1 1))
    (if (empty? args) frame nil)))

(defn clear [frame]
  (.clearRect (.getGraphics frame) 0 0 200 200))

(defn manipulator [f xs ys]
  (for [x (range xs) y (range ys)]
    [x y (rem (f x y) 256)]))

(defn draw [f xs ys & args]
  (let [frame (if (empty? args) (setup-frame) (first args))
        gfx (.getGraphics frame)]
    (clear frame)
    (doseq [[x y v] (manipulator f xs ys)]
      (.setColor gfx (java.awt.Color. v v v))
      (.fillRect gfx x y 1 1))))

(defn exp [x n]
  (reduce * (repeat n x)))
