(ns asteroids.core
  (:import (java.awt Graphics Graphics2D Color Dimension Polygon)
           (javax.swing JPanel JFrame Timer JOptionPane JFileChooser)
           (java.awt.image BufferedImage PixelGrabber)
           (java.io File)
           (javax.imageio ImageIO)
           (java.awt.event ActionListener KeyListener))
  #_(:use examples.import-static))
#_(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

; ----------------------------------------------------------
; functional model
; ----------------------------------------------------------
; START: constants
(def width 120)
(def height 100)
(def point-size 8)
(def ship-size 1.5)
(def shot-size (/ ship-size 10))
(def shot-speed 1.5)
(def shot-millis 250)
(def turn-millis 20)
(def turn-speed 4)
(def actions {
              (int \A) {:rotation (- turn-speed)}
              (int \D) {:rotation turn-speed}
              (int \W) {:acceleration 0.7}
              (int \K) {:shoot true}
              })

; END: constants

; transformation math:

(defn invert [mat]
  (apply map vector mat))

(defn mat*vec [m v]
  (let [vec*vec (fn [& vs] (reduce + (apply map * vs)))]
    (map #(vec*vec % v) m)))

(defn mat*mat [m1 m2]
  (invert (map (partial mat*vec m1) (invert m2))))

(defn rot [x]
  (let [cos-x (Math/cos x)
        sin-x (Math/sin x)]
    [[cos-x (- sin-x) 0]
     [sin-x cos-x 0]
     [0 0 1]]))

(defn trans [x y]
  [[1 0 x]
   [0 1 y]
   [0 0 1]])

(defn scl [s]
  [[s 0 0]
   [0 s 0]
   [0 0 1]])

(defn transpose-vec [x y v]
  (mat*vec (trans x y) v))

(defn rotate-vec [a v]
  (->> (conj v 1)
       (mat*vec (rot a))
       (take 2)))

(defn scale-vec [s v]
  (mat*vec (scl s) v))

(defn transpose-mat [x y m]
  (mat*mat (trans x y) m))

(defn rotate-mat [a m]
  (mat*mat (rot a) m))

(defn scale-mat [s m]
  (mat*mat (scl s) m))

(defn round [v]
  (map (fn [x] (if (integer? x) x (Math/round x))) v))

(defn vec+vec [& vs]
  (apply map + vs))

(defn num*vec [n v]
  (map (partial * n) v))

(defn get-transformation-matrix [{:keys [size position direction]} scale]
  (let [[x y] position]
    (->> (scl size)
         (rotate-mat direction)
         (transpose-mat x y)
         (scale-mat scale)))
  )

(defn polygon-to-screen [pol mat]
  (let [f (fn [v] (->> (conj v 1)
                       (mat*vec mat)
                       (take 2)
                       (round)))]
    (map f pol))
  )


(defn create-ship []
  {:type                   :ship

   :size                   ship-size
   :shape                  [[-0.143 1.0] [0.143 1.0] [0.429 0.429] [1.0 0.143] [1.0 -1.0] [0.429 -0.714]
                            [0.143 -1.0] [-0.143 -1.0] [-0.429 -0.714] [-1.0 -1.0] [-1.0 0.143] [-0.429 0.429]]
   :color                  (Color. 160 160 150)

   :cockpit-shape          [[-0.143 0.714] [0.143 0.714] [0.286 -0.143] [-0.286 -0.143]]
   :cockpit-color          (Color. 0 0 80)

   :shots                  []
   :shooting?              false
   :millis-since-last-shot shot-millis

   :position               [(/ width 2) (/ height 2)]
   :speed                  [0 0]
   :acceleration           0

   :direction              0
   :rotation-speed         0
   })

(defn create-shot
  ([pos dir speed]
   {:type           :shot

    :size           shot-size
    :shape          [[-1.0 -5.0] [1.0 -5.0] [0.0 1.0]]
    :color          (Color. 250 100 20)

    :position       pos
    :speed          speed
    :acceleration   0

    :direction      dir
    :rotation-speed 0
    })
  ([{:keys [position size direction speed] :as ship}]
   (let [dir-vec (rotate-vec direction [0 1])
         shot-pos (vec+vec position (num*vec size dir-vec))
         shot-speed (vec+vec speed (num*vec shot-speed dir-vec))]
     (create-shot shot-pos direction shot-speed))))

(defn out-of-bounds? [{[x y] :position size :size}]
  (or
    (< x (- size))
    (> x (+ width size))
    (< y (- size))
    (> y (+ height size))
    ))

(def win? (constantly false))

(defn lose? [ship]
  (out-of-bounds? ship))

; ----------------------------------------------------------------------
; Actions
; ----------------------------------------------------------------------

(defn shoot [{:keys [shooting? shots millis-since-last-shot] :as ship}]
  #_(prn "shoot" position size direction speed)
  (let [millis-since-last-shot (+ turn-millis millis-since-last-shot)
        shoots? (and shooting? (>= millis-since-last-shot shot-millis))
        shots (if shoots? (conj shots (create-shot ship)) shots)]
    (assoc ship :millis-since-last-shot (if shoots? 0 millis-since-last-shot)
                :shots shots))
  )

(defmulti move (fn [object & _] (:type object)))

(defmethod move :ship [{:keys [direction rotation-speed position speed acceleration shots] :as ship}]
  (let [direction (+ direction (* rotation-speed turn-millis 0.001))
        speed (vec+vec speed (rotate-vec direction [0 (* acceleration turn-millis 0.001)]))
        position (vec+vec speed position)
        shots (->> shots
                   (filter #(not (out-of-bounds? %)))
                   (map move))]
    (assoc ship :direction direction
                :speed speed
                :position position
                :shots shots
                )))

(defmethod move :shot [{:keys [position speed] :as shot}]
  (let [position (vec+vec speed position)]
    (assoc shot :position position)))

; Accelerating shots - not used right now
#_(defmethod move :shot [{:keys [position speed direction acceleration] :as shot}]
  (let [speed (vec+vec speed (rotate-vec direction [0 (* acceleration turn-millis 0.001)]))
        position (vec+vec speed position)]
    (assoc shot :speed speed
                :position position)))

(defn turn [{:keys [rotation-speed] :as ship} rot]
  (assoc ship :rotation-speed (+ rotation-speed rot)))

(defn accelerate [{:keys [acceleration] :as ship} acc]
  (assoc ship :acceleration (+ acceleration acc)))

(defn toggle-shooting [ship do-shoot?]
  (assoc ship :shooting? do-shoot?))

; ----------------------------------------------------------
; mutable model
; ----------------------------------------------------------
(defn update-positions! [ship]
  (dosync
    (alter ship shoot)
    (alter ship move))
  nil)

(defn do-action!
  ([ship {:keys [rotation acceleration shoot] :as action} modifier]
   (when rotation (dosync (alter ship turn (* modifier rotation))))
   (when acceleration (dosync (alter ship accelerate (* modifier acceleration))))
   (when shoot (dosync (alter ship toggle-shooting (> modifier 0))))
    #_(prn "update" dir modifier "speed:" (:rotation-speed @ship) "acceleration:" (:acceleration @ship)))
  ([ship action]
   (do-action! ship action 1))
  )

(defn reset-game! [ship]
  (dosync (ref-set ship (create-ship)))
  nil)

; ----------------------------------------------------------
; gui
; ----------------------------------------------------------

(defn draw-polygon [graphics polygon color]
  (doto graphics
    (.setColor color)
    (.fillPolygon (let [jpolygon (new Polygon)]
                    (doseq [[x y] polygon] (. jpolygon (addPoint x y)))
                    jpolygon)))
  nil)

(defmulti paint (fn [_ object] (:type object)))

(defmethod paint :ship [g {:keys [shape color cockpit-shape cockpit-color shots] :as ship}]
  (doseq [shot shots]
    (paint g shot))
  (let [transformation (get-transformation-matrix ship point-size)
        polygon (polygon-to-screen shape transformation)
        cockpit-polygon (polygon-to-screen cockpit-shape transformation)]
    (draw-polygon g polygon color)
    (draw-polygon g cockpit-polygon cockpit-color)))

(defmethod paint :shot [g {:keys [shape color] :as shot}]
  (let [transformation (get-transformation-matrix shot point-size)
        polygon (polygon-to-screen shape transformation)]
    (draw-polygon g polygon color)))

; ----------------------------------------------------------
; game
; ----------------------------------------------------------

(defn game-panel [frame ship]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]                                     ; <label id="code.game-panel.paintComponent"/>
      (proxy-super paintComponent g)
      (paint g @ship))
    (actionPerformed [e]                                    ; <label id="code.game-panel.actionPerformed"/>
      (update-positions! ship)
      (when (lose? @ship)
        (reset-game! ship)
        (JOptionPane/showMessageDialog frame "You lose!"))
      (when (win? @ship)
        (reset-game! ship)
        (JOptionPane/showMessageDialog frame "You win!"))
      (.repaint this))
    (keyPressed [e]
      (do-action! ship (actions (.getKeyCode e))))
    (keyReleased [e]
      (do-action! ship (actions (.getKeyCode e)) -1))
    (keyTyped [e])
    (getPreferredSize []
      (Dimension. (* (inc width) point-size)
                  (* (inc height) point-size)))))

(defn game []
  (let [ship (ref (create-ship))
        frame (JFrame. "Asteroids")
        panel (game-panel frame ship)
        timer (Timer. turn-millis panel)]
    (doto panel                                             ; <label id="code.game.panel"/>
      (.setFocusable true)
      (.addKeyListener panel)
      (.setBackground (Color. 0 0 0)))
    (doto frame                                             ; <label id="code.game.frame"/>
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)                                          ; <label id="code.game.timer"/>
    {:ship ship, :timer timer}))                            ; <label id="code.game.return"/>
