(ns asteroids.interop)

(defn play-sound [sound]
  (let [file (str "sounds/" sound ".wav")]
    (let [audio (js/Audio. file)]
      (.play audio))))

; ----------------------------------------------------------
; gui
; ----------------------------------------------------------

(defn- js-color [{:keys [r g b a]}]
  (str "rgba(" r ", " g ", " b ", " (/ a 255) ")"))

(defn- paint-circle [graphics [[x1 y1] [x2 y2]]]
  (let [x (quot (+ x1 x2) 2)
        y (quot (+ y1 y2) 2)
        r (js/Math.abs (quot (- x2 x1) 2))]
    (.beginPath graphics)
    (.arc graphics x y r 0 (* 2 js/Math.PI))
    (.closePath graphics)))

(defn draw-circle [graphics polygon color]
  (set! (.-strokeStyle graphics) (js-color color))
  (paint-circle graphics polygon)
  (.stroke graphics))

(defn fill-circle [graphics polygon color]
  (set! (.-fillStyle graphics) (js-color color))
  (paint-circle graphics polygon)
  (.fill graphics))

(defn- paint-polygon [graphics [[x y] & rest]]
  (.beginPath graphics)
  (.moveTo graphics x y)
  (doseq [[x y] rest]
    (.lineTo graphics x y))
  (.closePath graphics))

(defn draw-polygon [graphics polygon color]
  (set! (.-strokeStyle graphics) (js-color color))
  (paint-polygon graphics polygon)
  (.stroke graphics))

(defn fill-polygon [graphics polygon color]
  (set! (.-fillStyle graphics) (js-color color))
  (paint-polygon graphics polygon)
  (.fill graphics))

