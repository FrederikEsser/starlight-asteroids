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
(def sqrt3 (Math/sqrt 3.0))

(def width 120)
(def height 100)
(def point-size 8)
(def ship-size 1.5)
(def ship-life 3)
(def turn-speed 4)
(def shot-size (/ ship-size 10))
(def shot-speed 1.5)
(def shot-millis 200)
(def asteroids-start-number 1)
(def asteroid-start-size 5)
(def asteroid-min-size 1.2)
(def asteroid-size-variance 0.2)
(def asteroid-jaggedness 0.6)
(def asteroid-max-speed 0.6)
(def asteroid-max-rotation-speed 4)
(def turn-millis 40)

(def actions {
              (int \A) {:rotation (- turn-speed)}
              (int \D) {:rotation turn-speed}
              (int \W) {:acceleration 1}
              (int \K) {:shoot true}
              })

; END: constants

(defn random [from to]
  (let [range (- to from)]
    (+ from (rand range))))

(defn vary [variance n]
  (let [from (* n (- 1 variance))
        to (* n (+ 1 variance))]
    (random from to)))

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
  (let [f (fn [v] (->> (conj (vec v) 1)
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

   :life                   ship-life
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

(defn create-asteroid
  ([size pos speed]
   (let [direction (rand (* 2 (Math/PI)))
         dir-vec (rotate-vec direction [0 1])
         speed (vec+vec speed (num*vec (rand asteroid-max-speed) dir-vec))
         position (vec+vec pos (num*vec size dir-vec))
         rotation-speed (rand asteroid-max-rotation-speed)
         num-points (Math/round (+ size 3.0))
         make-point (fn [n]
                      (let [angle (* 2 (Math/PI) (/ n num-points))
                            length (random (- 1 asteroid-jaggedness) (+ 1 (/ asteroid-jaggedness 2)))]
                        (rotate-vec angle [0 length])))
         shape (map make-point (range num-points))]
     {:type           :asteroid

      :size           (vary asteroid-size-variance size)
      :shape          shape
      :color          (Color. 250 240 20)

      :position       position
      :speed          speed
      :acceleration   0

      :direction      direction
      :rotation-speed rotation-speed
      }))
  ([size pos]
   (create-asteroid size pos [0 0]))
  ([size]
   (let [x (rand width)
         y (rand height)]
     (create-asteroid size [x y]))))

(defn create-asteroids
  ([num size] (map create-asteroid (repeat num size)))
  ([num size pos speed] (map create-asteroid (repeat num size) (repeat num pos) (repeat num speed)))
  #_(repeat num (create-asteroid size)))

(defn out-of-bounds? [{[x y] :position size :size}]
  (or
    (< x (- size))
    (> x (+ width size))
    (< y (- size))
    (> y (+ height size))
    ))

(defn collide? [{size1 :size [x1 y1] :position} {size2 :size [x2 y2] :position}]
  (let [distance (fn [a b max]
                   (let [d (Math/abs (- a b))]
                     (if (< d (/ max 2)) d (- max d))))
        x (distance x1 x2 width)
        y (distance y1 y2 height)
        size (+ size1 size2)
        square #(* % %)]
    (<= (+ (square x) (square y)) (square size))))

(defmulti check-collision
          (fn [obj1 obj2]
            (cond
              (and (map? obj1) (map? obj2)) :map-map
              (and (map? obj1) (sequential? obj2)) :map-seq
              (and (sequential? obj1) (sequential? obj2)) :seq-seq)))

(defmethod check-collision :map-map [obj1 obj2]
  (if (collide? obj1 obj2)
    [(assoc obj1 :hit? true) (assoc obj2 :hit? true)]
    [obj1 obj2]))

(defmethod check-collision :map-seq [obj1 coll]
  (loop [obj1 obj1, coll coll, coll-result []]
    (if (empty? coll)
      [obj1 coll-result]
      (let [[obj1 obj2] (check-collision obj1 (first coll))]
        (recur obj1 (rest coll) (conj coll-result obj2)))
      )))

(defmethod check-collision :seq-seq [coll1 coll2]
  (loop [coll1 coll1, coll2 coll2, coll1-result []]
    (if (empty? coll1)
      [coll1-result coll2]
      (let [[obj1 coll2] (check-collision (first coll1) coll2)]
        (recur (rest coll1) coll2 (conj coll1-result obj1)))
      )))

(defmulti handle-collision (fn [object] (:type object)))

(defmethod handle-collision :ship [{:keys [hit? life] :as ship}]
  (if hit?
    (assoc ship :life (dec life)
                :hit? false)
    ship))

(defmethod handle-collision :shot [{:keys [hit?] :as shot}]
  (if hit?
    nil
    shot))

(defmethod handle-collision :asteroid [{:keys [hit? size position speed] :as asteroid}]
  (let [fragment-size (/ size sqrt3)]
    (if hit?
      (if (< fragment-size asteroid-min-size)
        nil
        (create-asteroids 3 (/ size sqrt3) position speed))
      asteroid)))

(defn resolve-all-collision [{shots :shots :as ship} asteroids]
  (let [[ship asteroids] (check-collision ship asteroids)
        [shots asteroids] (check-collision shots asteroids)
        ship (handle-collision ship)
        shots (->> shots
                   (map handle-collision)
                   flatten
                   (filter (comp not nil?)))
        asteroids (->> asteroids
                       (map handle-collision)
                       flatten
                       (filter (comp not nil?)))]
    [(assoc ship :shots shots) asteroids]))

(def win? (constantly false))
(def lose? (constantly false))

(defn win? [asteroids]
  (empty? asteroids))

(defn lose? [{life :life}]
  (<= life 0))

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

(defmulti move (fn [object] (:type object)))

(defmethod move :ship [{:keys [direction rotation-speed position speed acceleration shots] :as ship}]
  (let [direction (+ direction (* rotation-speed turn-millis 0.001))
        speed (vec+vec speed (rotate-vec direction [0 (* acceleration turn-millis 0.001)]))
        [x y] (vec+vec speed position)
        position [(mod x width) (mod y height)]
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

(defmethod move :asteroid [{:keys [direction rotation-speed position speed] :as asteroid}]
  (let [direction (+ direction (* rotation-speed turn-millis 0.001))
        [x y] (vec+vec speed position)
        position [(mod x width) (mod y height)]]
    (assoc asteroid :direction direction
                    :position position
                    )))

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
(defn update-positions! [ship asteroids]
  (dosync
    (alter ship shoot)
    (alter ship move)
    (alter asteroids (partial map move))
    nil))

(defn resolve-collision! [ship asteroids]
  (dosync
    (let [[ship* asteroids*] (resolve-all-collision @ship @asteroids)]
      (ref-set ship ship*)
      (ref-set asteroids asteroids*))))

(defn do-action!
  ([ship {:keys [rotation acceleration shoot] :as action} modifier]
   (when rotation (dosync (alter ship turn (* modifier rotation))))
   (when acceleration (dosync (alter ship accelerate (* modifier acceleration))))
   (when shoot (dosync (alter ship toggle-shooting (> modifier 0))))
    #_(prn "update" dir modifier "speed:" (:rotation-speed @ship) "acceleration:" (:acceleration @ship)))
  ([ship action]
   (do-action! ship action 1))
  )

(defn reset-game! [ship asteroids level increase-level?]
  (dosync
    (ref-set ship (create-ship))
    (when increase-level?
      (alter level inc))
    (ref-set asteroids (create-asteroids @level asteroid-start-size)))
  nil)

; ----------------------------------------------------------
; gui
; ----------------------------------------------------------

(defn draw-polygon [graphics polygon color]
  (let [jpolygon (new Polygon)]
    (doseq [[x y] polygon] (. jpolygon (addPoint x y)))
    (doto graphics
      (.setColor color)
      (.drawPolygon jpolygon)))
  nil)

(defn fill-polygon [graphics polygon color]
  (let [jpolygon (new Polygon)]
    (doseq [[x y] polygon] (. jpolygon (addPoint x y)))
    (doto graphics
      (.setColor color)
      (.fillPolygon jpolygon)))
  nil)

(defmulti paint (fn [_ object] (:type object)))

(defmethod paint :ship [g {:keys [shape color cockpit-shape cockpit-color shots] :as ship}]
  (doseq [shot shots]
    (paint g shot))
  (let [transformation (get-transformation-matrix ship point-size)
        polygon (polygon-to-screen shape transformation)
        cockpit-polygon (polygon-to-screen cockpit-shape transformation)]
    (fill-polygon g polygon color)
    (fill-polygon g cockpit-polygon cockpit-color)))

(defmethod paint :shot [g {:keys [shape color] :as object}]
  (let [transformation (get-transformation-matrix object point-size)
        polygon (polygon-to-screen shape transformation)]
    (fill-polygon g polygon color)))

(defmethod paint :asteroid [g {:keys [shape color] :as object}]
  (let [transformation (get-transformation-matrix object point-size)
        polygon (polygon-to-screen shape transformation)]
    (draw-polygon g polygon color)))

; ----------------------------------------------------------
; game
; ----------------------------------------------------------

(defn game-panel [frame ship asteroids level]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]                                     ; <label id="code.game-panel.paintComponent"/>
      (proxy-super paintComponent g)
      (paint g @ship)
      (doseq [asteroid @asteroids]
        (paint g asteroid)))
    (actionPerformed [e]                                    ; <label id="code.game-panel.actionPerformed"/>
      (update-positions! ship asteroids)
      (resolve-collision! ship asteroids)
      (when (lose? @ship)
        (reset-game! ship asteroids level false)
        (JOptionPane/showMessageDialog frame "You lose!"))
      (when (win? @asteroids)
        (reset-game! ship asteroids level true)
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
        level (ref asteroids-start-number)
        asteroids (ref (create-asteroids asteroids-start-number asteroid-start-size))
        frame (JFrame. "Asteroids")
        panel (game-panel frame ship asteroids level)
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
