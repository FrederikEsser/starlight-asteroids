(ns asteroids.matrix-math)

; transformation math:

(defn invert [mat]
  (apply map vector mat))

(defn mat*vec [m v]
  (let [vec*vec (fn [& vs] (reduce + (apply map * vs)))]
    (map #(vec*vec % v) m)))

(defn mat*mat [m1 m2]
  (invert (map (partial mat*vec m1) (invert m2))))

(defn rotate [x]
  (let [cos-x (Math/cos x)
        sin-x (Math/sin x)]
    [[cos-x (- sin-x) 0]
     [sin-x cos-x 0]
     [0 0 1]]))

(defn transpose [x y]
  [[1 0 x]
   [0 1 y]
   [0 0 1]])

(defn scale [s]
  [[s 0 0]
   [0 s 0]
   [0 0 1]])

(defn transpose-vec [x y v]
  (mat*vec (transpose x y) v))

(defn rotate-vec [a v]
  (->> (conj v 1)
       (mat*vec (rotate a))
       (take 2)))

(defn scale-vec [s v]
  (mat*vec (scale s) v))

(defn transpose-mat [x y m]
  (mat*mat (transpose x y) m))

(defn rotate-mat [a m]
  (mat*mat (rotate a) m))

(defn scale-mat [s m]
  (mat*mat (scale s) m))

(defn round [v]
  (map (fn [x] (if (integer? x) x (Math/round x))) v))

(defn vec+vec [& vs]
  (apply map + vs))

(defn num*vec [n v]
  (map (partial * n) v))

