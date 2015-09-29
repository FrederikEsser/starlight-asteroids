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
(def point-size 8.5)
(def ship-size 1.5)
(def ship-life 3)
(def turn-speed 4)
(def shot-size (/ ship-size 10))
(def shot-speed 2.5)
(def shot-millis 400)
(def asteroids-start-number 1)
(def asteroid-start-size 5)
(def asteroid-min-size 1.2)
(def asteroid-size-variance 0.2)
(def asteroid-jaggedness 0.6)
(def asteroid-max-speed 0.6)
(def asteroid-max-rotation-speed 4)
(def debris-life-millis 3000)
(def turn-millis 40)

(def actions {
              (int \A) {:rotation (- turn-speed)}
              (int \D) {:rotation turn-speed}
              (int \W) {:acceleration 1}
              (int \K) {:shoot true}
              (int \R) {:repair true}
              (int \1) {:set-level 1}
              (int \2) {:set-level 2}
              (int \3) {:set-level 3}
              (int \4) {:set-level 4}
              (int \5) {:set-level 5}
              (int \6) {:set-level 6}
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

(declare fill-polygon draw-polygon)

(defn create-ship []
  {:type                   :ship

   :size                   ship-size
   :shape                  [[-0.143 1.0] [0.143 1.0] [0.429 0.429] [1.0 0.143] [1.0 -1.0] [0.429 -0.714]
                            [0.143 -1.0] [-0.143 -1.0] [-0.429 -0.714] [-1.0 -1.0] [-1.0 0.143] [-0.429 0.429]]
   :color                  (Color. 160 160 150)
   :paint-method fill-polygon

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
   :life-colors            [(Color. 255 0 0) (Color. 255 80 75) (Color. 250 160 75) (Color. 160 160 150)]
   :collidable?            false                            ; is set to true at first movement / shooting
   })

(defn create-shot
  ([pos dir speed]
   {:type           :shot

    :size           shot-size
    :shape          [[-1.0 -5.0] [1.0 -5.0] [0.0 1.0]]
    :color          (Color. 250 50 10)
    :paint-method fill-polygon

    :position       pos
    :speed          speed
    :acceleration   0

    :direction      dir
    :rotation-speed 0

    :collidable?    true
    })
  ([{:keys [position size direction speed] :as ship}]
   (let [dir-vec (rotate-vec direction [0 1])
         shot-pos (vec+vec position (num*vec size dir-vec))
         shot-speed (vec+vec speed (num*vec shot-speed dir-vec))]
     (create-shot shot-pos direction shot-speed))))

(defn create-asteroid
  ([size pos speed]
   (let [size (vary asteroid-size-variance size)
         direction (rand (* 2 (Math/PI)))
         dir-vec (rotate-vec direction [0 1])
         speed (vec+vec speed (num*vec (rand asteroid-max-speed) dir-vec))
         position (vec+vec pos (num*vec size dir-vec))
         rotation-speed (rand asteroid-max-rotation-speed)
         num-points (Math/round (+ size 3.0))
         make-point (fn [n]
                      (let [angle (* 2 (Math/PI) (/ n num-points))
                            length (random (- 1 asteroid-jaggedness) (+ 1 (/ asteroid-jaggedness 2)))]
                        (rotate-vec angle [0 length])))
         shape (map make-point (range num-points))
         debris? (< size asteroid-min-size)]
     {:type           (if debris? :debris :asteroid)

      :size           (vary asteroid-size-variance size)
      :shape          shape
      :color          (Color. 250 240 (if debris? 150 20))
      :paint-method draw-polygon

      :position       position
      :speed          speed
      :acceleration   0

      :direction      direction
      :rotation-speed rotation-speed

      :collidable?    (not debris?)

      :life-time      (if debris? debris-life-millis nil)
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
  #_(repeat num (create-asteroid size pos speed)))

(defn out-of-bounds? [{[x y] :position size :size}]
  (or
    (< x (- size))
    (> x (+ width size))
    (< y (- size))
    (> y (+ height size))
    ))

(defn collide? [{size1 :size [x1 y1] :position} {size2 :size [x2 y2] :position}]
  (let [distance (fn [a b max] (let [d (Math/abs (- a b))] (if (< d (/ max 2)) d (- max d))))
        delta-x (- x1 x2)
        delta-y (- y1 y2)
        size (+ size1 size2)
        square #(* % %)]
    (<= (+ (square delta-x) (square delta-y)) (square size))))

(defmulti check-collision
          (fn [obj1 obj2]
            (cond
              (and (map? obj1) (map? obj2)) :map-map
              (and (map? obj1) (sequential? obj2)) :map-seq
              (and (sequential? obj1) (sequential? obj2)) :seq-seq)))

(defmethod check-collision :map-map [obj1 obj2]
  (if (and (:collidable? obj1)
           (:collidable? obj2)
           (collide? obj1 obj2))
    [(assoc obj1 :hit? true) (assoc obj2 :hit? true)]
    [obj1 obj2]))

(defmethod check-collision :map-seq [obj1 coll]
  (loop [obj1 obj1, coll coll, coll-result []]
    (if (empty? coll)
      [obj1 coll-result]
      (let [[obj1 obj2] (check-collision obj1 (first coll))]
        (recur obj1 (rest coll) (conj coll-result obj2))))))

(defmethod check-collision :seq-seq [coll1 coll2]
  (loop [coll1 coll1, coll2 coll2, coll1-result []]
    (if (empty? coll1)
      [coll1-result coll2]
      (let [[obj1 coll2] (check-collision (first coll1) coll2)]
        (recur (rest coll1) coll2 (conj coll1-result obj1))))))

(defmulti handle-collision (fn [{type :type}]
                             (if (#{:ship :shot :asteroid} type)
                               type
                               :other)))

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
      (create-asteroids 3 fragment-size position speed)
      asteroid)))

(defmethod handle-collision :other [{hit? :hit? :as object}]
  (if hit?
    nil
    object))

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

(defn win? [asteroids]
  (empty? asteroids))

(defn lose? [{life :life}]
  (<= life 0))

; ----------------------------------------------------------------------
; Actions
; ----------------------------------------------------------------------

(defn repair-ship [ship]
  "Repair method while input keys f*** up at start-up"
  (assoc ship :rotation-speed 0
              :acceleration 0
              :shooting? false))

(defn shoot [{:keys [shooting? shots millis-since-last-shot] :as ship}]
  (let [millis-since-last-shot (+ turn-millis millis-since-last-shot)
        shoots? (and shooting? (>= millis-since-last-shot shot-millis))
        shots (if shoots? (conj shots (create-shot ship)) shots)]
    (assoc ship :millis-since-last-shot (if shoots? 0 millis-since-last-shot)
                :shots shots))
  )

(defn basic-move
  ([{:keys [direction rotation-speed speed acceleration position] :as object} wrap?]
   (let [direction (+ direction (* rotation-speed turn-millis 0.001))
         speed (vec+vec speed (rotate-vec direction [0 (* acceleration turn-millis 0.001)]))
         [x y] (vec+vec speed position)]
     (assoc object :direction direction
                   :speed speed
                   :position (if wrap? [(mod x width) (mod y height)] [x y]))))
  ([object] (basic-move object true)))

(defmulti move (fn [object] (:type object)))

(defmethod move :ship [{shots :shots :as ship}]
  (let [ship (basic-move ship)
        shots (->> shots
                   (filter #(not (out-of-bounds? %)))
                   (map move))]
    (assoc ship :shots shots)))

(defmethod move :shot [shot]
  (basic-move shot false))

(defmethod move :asteroid [asteroid]
  (basic-move asteroid))

(defmethod move :debris [{:keys [life-time size] :as debris}]
  (if (<= life-time 0)
    nil
    (let [debris (basic-move debris)
          size (* size (- 1 (/ turn-millis life-time)))
          life-time (- life-time turn-millis)]
      (assoc debris :size size
                    :life-time life-time))))

(defn move-many [coll]
  (->> coll
       (map move)
       (filter (comp not nil?))))

(defn turn [{:keys [rotation-speed] :as ship} rot]
  (assoc ship :rotation-speed (+ rotation-speed rot)))

(defn accelerate [{:keys [acceleration] :as ship} acc]
  (assoc ship :acceleration (+ acceleration acc)
              :collidable? true))

(defn toggle-shooting [ship do-shoot?]
  (assoc ship :shooting? do-shoot?
              :collidable? true))

; ----------------------------------------------------------
; mutable model
; ----------------------------------------------------------
(defn update-positions! [ship asteroids]
  (dosync
    (alter ship shoot)
    (alter ship move)
    (alter asteroids move-many)
    nil))

(defn resolve-collision! [ship asteroids]
  (dosync
    (let [[ship* asteroids*] (resolve-all-collision @ship @asteroids)]
      (ref-set ship ship*)
      (ref-set asteroids asteroids*))))

(defn do-action!
  ([ship {:keys [rotation acceleration shoot repair] :as action} modifier]
   (when rotation (dosync (alter ship turn (* modifier rotation))))
   (when acceleration (dosync (alter ship accelerate (* modifier acceleration))))
   (when shoot (dosync (alter ship toggle-shooting (> modifier 0))))
   (when repair (dosync (alter ship repair-ship))))
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

(defmulti paint (fn [_ {type :type}]
                  (if (= type :ship) :ship :other)))

(defmethod paint :ship [g {:keys [shape life-colors life cockpit-shape cockpit-color shots paint-method] :as ship}]
  (doseq [shot shots]
    (paint g shot))
  (let [transformation (get-transformation-matrix ship point-size)
        polygon (polygon-to-screen shape transformation)
        color (get life-colors life)
        cockpit-polygon (polygon-to-screen cockpit-shape transformation)]
    (paint-method g polygon color)
    (paint-method g cockpit-polygon cockpit-color)))

(defmethod paint :other [g {:keys [shape color paint-method] :as object}]
  (let [transformation (get-transformation-matrix object point-size)
        polygon (polygon-to-screen shape transformation)]
    (paint-method g polygon color)))

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
    (def ship ship)
    (def timer timer)
    (def frame frame)
    {:ship ship}))                                          ; <label id="code.game.return"/>
