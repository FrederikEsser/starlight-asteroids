(ns asteroids.core
  (:import (java.awt Graphics Graphics2D Color Dimension Polygon)
           (javax.swing JPanel JFrame Timer JOptionPane JFileChooser)
           (java.awt.image BufferedImage PixelGrabber)
           (java.io File)
           (javax.imageio ImageIO)
           (java.awt.event ActionListener KeyListener)
           (javax.sound.sampled AudioSystem FloatControl$Type))
  (:require [asteroids.matrix-math :as m])
  (:use clojure.java.io))
#_(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

; ----------------------------------------------------------
; functional model
; ----------------------------------------------------------

(def width 160)
(def height 85)
(def point-size 8.5)
(def turn-millis 40)

(def ship-size 1.5)
(def ship-life 3)
(def ship-acceleration 1)
(def ship-rotation-speed 4)
(def ship-damage 4)

(def asteroids-start-number 1)
(def asteroid-start-size 5)
(def asteroid-min-size 1.3)
(def asteroid-size-variance 0.1)
(def asteroid-jaggedness 0.7)
(def asteroid-max-speed 0.6)
(def asteroid-max-rotation-speed 4)
(def asteroid-damage 1)
(def debris-life-millis 3000)

(defn random-int [from to]
  (let [range (- to from)]
    (+ from (rand-int range))))

(defn random [from to]
  (let [range (- to from)]
    (+ from (rand range))))

(defn vary [variance n]
  (let [from (* n (- 1 variance))
        to (* n (+ 1 variance))]
    (random from to)))

(def ammunition-types {:projectile {:size         (/ ship-size 10)
                                    :shape        [[-1.0 -5.0] [1.0 -5.0] [0.0 1.0]]
                                    :color        (Color. 250 50 10)
                                    :speed        2.5
                                    :acceleration 0
                                    :damage       3
                                    :cooldown     300
                                    :explosion    {:size      3
                                                   :color     (Color. 220 180 180)
                                                   :life-time 400}}
                       :fire       {:size           (/ ship-size 2.5)
                                    :shape          [[-1.0 -1.0] [1.0 -1.0] [0.0 1.0]]
                                    :color          (fn [] (Color. (random-int 230 256) (random-int 60 220) (random-int 0 50)))
                                    :spread         0.2
                                    :speed          -0.7
                                    :acceleration   0
                                    :rotation-speed (fn [] (random -5 5))
                                    :cooldown       30
                                    :life-time      400}
                       :missile    {:size         (/ ship-size 3)
                                    :shape        [[0.0 1.0] [0.5 -1.0] [0.5 -3.5] [1.0 -4.5] [0.5 -4.5] [0.25 -4.25]
                                                   [-0.25 -4.25] [-0.5 -4.5] [-1.0 -4.5] [-0.5 -3.5] [-0.5 -1.0]]
                                    :color        (Color. 10 50 250)
                                    :speed        0
                                    :acceleration 3
                                    :damage       30
                                    :cooldown     10000
                                    :explosion    {:size      30
                                                   :color     (Color. 230 230 250)
                                                   :life-time 200}}})

(def actions {
              (int \A) {:rotation (- ship-rotation-speed)}
              (int \D) {:rotation ship-rotation-speed}
              (int \W) {:acceleration ship-acceleration}
              (int \J) {:fire-weapon :launcher}
              (int \K) {:fire-weapon :machinegun}})

(def sqrt (memoize #(Math/sqrt %)))

(defn get-transformation-matrix [{:keys [size position direction]} scale]
  (let [[x y] position]
    (->> (m/scale size)
         (m/rotate-mat direction)
         (m/transpose-mat x y)
         (m/scale-mat scale)))
  )

(defn polygon-to-screen [pol mat]
  (let [transform (fn [v] (->> (conj (vec v) 1)
                               (m/mat*vec mat)
                               (take 2)
                               (m/round)))]
    (map transform pol))
  )

(declare fill-polygon draw-polygon fill-circle draw-circle)

(defn create-weapon [ammunition-type]
  {:ammunition-type ammunition-type
   :cooldown        0
   :shooting?       false})

(defn create-ship []
  {:type           :ship

   :size           ship-size
   :shape          [[-0.143 1.0] [0.143 1.0] [0.429 0.429] [1.0 0.143] [1.0 -1.0] [0.429 -0.714]
                    [0.143 -1.0] [-0.143 -1.0] [-0.429 -0.714] [-1.0 -1.0] [-1.0 0.143] [-0.429 0.429]]
   :color          (Color. 160 160 150)
   :paint-method   fill-polygon

   :cockpit-shape  [[-0.143 0.714] [0.143 0.714] [0.286 -0.143] [-0.286 -0.143]]
   :cockpit-color  (Color. 0 0 80)

   :weapons        {:engine     (create-weapon :fire)
                    :machinegun (create-weapon :projectile)
                    :launcher   (create-weapon :missile)}
   :shots          []

   :position       [(/ width 2) (/ height 2)]
   :speed          [0 0]
   :acceleration   0

   :direction      0
   :rotation-speed 0
   :rotation       #{}

   :life           ship-life
   :life-colors    {0 (Color. 255 0 0)
                    1 (Color. 255 80 75)
                    2 (Color. 250 160 75)
                    3 (Color. 160 160 150)}
   :collidable?    false                                    ; is set to ship-damage at first movement / shooting
   })

(defn get-expr-value
  ([expr default]
   (cond
     (nil? expr) default
     (fn? expr) (expr)
     :else expr))
  ([expr] (get-expr-value expr nil)))

(defn create-shot ([ammunition-type {ship-pos :position ship-size :size ship-dir :direction ship-speed :speed}]
   (let [{:keys [spread size shape color acceleration direction rotation-speed damage life-time]} (get ammunition-types ammunition-type)
         move-dir (if spread (+ ship-dir (random (- spread) spread)) ship-dir)
         dir-vec (m/rotate-vec move-dir [0 1])
         position (m/vec+vec ship-pos (m/num*vec ship-size dir-vec))
         speed (m/vec+vec ship-speed (m/num*vec (-> ammunition-types ammunition-type :speed) dir-vec))]
     {:type           ammunition-type

      :size           (get-expr-value size)
      :shape          (get-expr-value shape)
      :color          (get-expr-value color)
      :paint-method   fill-polygon

      :position       position
      :speed          speed
      :acceleration   (get-expr-value acceleration 0)

      :direction      (get-expr-value direction move-dir)
      :rotation-speed (get-expr-value rotation-speed 0)

      :collidable?    (get-expr-value damage)

      :life-time      (get-expr-value life-time)})))

(defn create-explosion [position {:keys [size color life-time damage] :as explosion}]
  (if explosion
    {:type           :explosion

     :size           size
     :shape          [[-1 -1] [1 1]]

     :color          color
     :paint-method   fill-circle

     :position       position
     :speed          [0 0]
     :acceleration   0

     :direction      0
     :rotation-speed 0

     :collidable?    damage

     :life-time      life-time
     }
    nil))

(defn create-asteroid
  ([size pos speed]
   (let [size (vary asteroid-size-variance size)
         direction (rand (* 2 (Math/PI)))
         dir-vec (m/rotate-vec direction [0 1])
         speed (m/vec+vec speed (m/num*vec (rand asteroid-max-speed) dir-vec))
         position (m/vec+vec pos (m/num*vec size dir-vec))
         rotation-speed (rand asteroid-max-rotation-speed)
         num-points (Math/round (+ size 3.0))
         make-point (fn [n]
                      (let [angle (* 2 (Math/PI) (/ n num-points))
                            length (random (- 1 asteroid-jaggedness) (+ 1 (/ asteroid-jaggedness 2)))]
                        (m/rotate-vec angle [0 length])))
         shape (map make-point (range num-points))
         debris? (< size asteroid-min-size)]
     {:type           (if debris? :debris :asteroid)

      :size           size
      :shape          shape
      :color          (Color. 250 240 (if debris? 150 20))
      :paint-method   draw-polygon

      :position       position
      :speed          speed
      :acceleration   0

      :direction      direction
      :rotation-speed rotation-speed

      :collidable?    (if debris? false asteroid-damage)

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

(defn play-sound
  ([sound gain]
   (let [file (str "resources/" sound ".wav")]
     (when (.exists (as-file file))
       (let [audio-stream (AudioSystem/getAudioInputStream (File. file))
             clip (AudioSystem/getClip)
             ;volume (.getControl clip FloatControl$Type/MASTER_GAIN)
             ]
         #_(doto volume
             (.setValue gain))
         (doto clip
           (.open audio-stream)
           (.setFramePosition 0)
           (.start))
         ))))
  ([sound] (play-sound sound 0)))

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

(defn damage [{current-damage :damage-taken :as obj} damage-amount]
  (assoc obj :damage-taken (if current-damage (+ current-damage damage-amount) damage-amount)))

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
    [(damage obj1 (:collidable? obj2)) (damage obj2 (:collidable? obj1))]
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

(defmulti handle-damage (fn [{type :type}]
                          (cond
                            (#{:projectile :missile} type) :shot
                            (#{:ship :asteroid} type) type
                            :else :other)))

(defmethod handle-damage :ship [{:keys [damage-taken life] :as ship}]
  (when damage-taken (play-sound (if (<= life damage-taken) "ship-destroyed" "ship-hit")))
  (if damage-taken
    (assoc ship :life (- life damage-taken)
                :damage-taken nil)
    ship))

(defmethod handle-damage :shot [{:keys [type damage-taken position] :as shot}]
  (when damage-taken (play-sound (str (name type) "-hit")))
  (if damage-taken
    (create-explosion position (-> ammunition-types type :explosion))
    shot))

(defmethod handle-damage :asteroid [{:keys [damage-taken size position speed] :as asteroid}]
  (if damage-taken
    (create-asteroids damage-taken (/ size (sqrt damage-taken)) position speed)
    asteroid))

(defmethod handle-damage :other [object]
  object)

(defn resolve-all-collision [{shots :shots :as ship} asteroids]
  (let [[ship asteroids] (check-collision ship asteroids)
        [shots asteroids] (check-collision shots asteroids)
        ship (handle-damage ship)
        shots (->> shots
                   (map handle-damage)
                   flatten
                   (filter (comp not nil?)))
        asteroids (->> asteroids
                       (map handle-damage)
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

(defn toggle-shooting [{:keys [weapons] :as ship} weapon-type activate?]
  (if (contains? weapons weapon-type)
    (-> ship
        (assoc-in [:weapons weapon-type :shooting?] activate?)
        (assoc :collidable? ship-damage))
    ship))

(defn fire-weapon [{:keys [shots] :as ship}
                   [weapon-type {:keys [ammunition-type cooldown shooting?] :as weapon}]]
  (let [cooldown (if (> cooldown turn-millis) (- cooldown turn-millis) 0)
        shoots? (and shooting? (= cooldown 0))
        cooldown (if shoots? (-> ammunition-types ammunition-type :cooldown) cooldown)
        shots (if shoots? (conj shots (create-shot ammunition-type ship)) shots)]
    (when shoots? (play-sound (str (name ammunition-type) "-fired")))
    (-> ship
        (assoc-in [:weapons weapon-type :cooldown] cooldown)
        (assoc :shots shots))))

(defn shoot [{:keys [weapons] :as ship}]
  (reduce fire-weapon ship weapons))

(defn basic-move
  ([{:keys [direction rotation-speed speed acceleration position] :as object} wrap?]
   (let [direction (+ direction (* rotation-speed turn-millis 0.001))
         speed (m/vec+vec speed (m/rotate-vec direction [0 (* acceleration turn-millis 0.001)]))
         [x y] (m/vec+vec speed position)]
     (assoc object :direction direction
                   :speed speed
                   :position (if wrap? [(mod x width) (mod y height)] [x y]))))
  ([object] (basic-move object true)))

(defmulti move :type)

(defmethod move :ship [{shots :shots :as ship}]
  (let [ship (basic-move ship)
        shots (->> shots
                   (filter (comp not out-of-bounds?))
                   (map move)
                   (filter (comp not nil?)))]
    (assoc ship :shots shots)))

(defmethod move :projectile [projectile]
  (basic-move projectile false))

(defmethod move :missile [missile]
  (basic-move missile false))

(defmethod move :fire [{:keys [life-time color] :as fire}]
  (if (< life-time turn-millis)
    nil
    (let [fire (basic-move fire false)
          alpha (int (* (.getAlpha color) (- 1 (/ turn-millis life-time))))
          color (Color. (.getRed color) (.getGreen color) (.getBlue color) alpha)
          life-time (- life-time turn-millis)]
      (assoc fire :life-time life-time
                  :color color))))

(defmethod move :asteroid [asteroid]
  (basic-move asteroid))

(defmethod move :debris [{:keys [life-time size] :as debris}]
  (if (< life-time turn-millis)
    nil
    (let [debris (basic-move debris)
          size (* size (- 1 (/ turn-millis life-time)))
          life-time (- life-time turn-millis)]
      (assoc debris :size size
                    :life-time life-time))))

(defmethod move :explosion [{:keys [life-time size color] :as explosion}]
  (if (< life-time turn-millis)
    nil
    (let [explosion (basic-move explosion)
          size (* size (- 1 (/ turn-millis life-time)))
          alpha (int (* (.getAlpha color) (- 1 (/ turn-millis life-time))))
          color (Color. (.getRed color) (.getGreen color) (.getBlue color) alpha)
          life-time (- life-time turn-millis)]
      (assoc explosion :size size
                       :life-time life-time
                       :color color))))

(defn move-many [coll]
  (->> coll
       (map move)
       (filter (comp not nil?))))

(defn set-rotation [{rotation :rotation :as ship} value activate?]
  (let [rotation (if activate?
                   (conj rotation value)
                   (disj rotation value))]
    (assoc ship :rotation rotation
                :rotation-speed (reduce + rotation))))

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
  ([ship {:keys [acceleration rotation fire-weapon] :as action} activate?]
   (when acceleration (dosync (alter ship assoc :acceleration (if activate? acceleration 0) :collidable? ship-damage)
                              (alter ship toggle-shooting :engine activate?)))
   (when rotation (dosync (alter ship set-rotation rotation activate?)))
   (when fire-weapon (dosync (alter ship toggle-shooting fire-weapon activate?))))
  ([ship action]
   (do-action! ship action true)))

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

(defn draw-circle [graphics [[x y] [x1 y1]] color]
  (doto graphics
    (.setColor color)
    (.drawOval x y (- x1 x) (- y1 y)))
  nil)

(defn fill-circle [graphics [[x y] [x1 y1]] color]
  (doto graphics
    (.setColor color)
    (.fillOval x y (- x1 x) (- y1 y)))
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
      (doseq [asteroid @asteroids]
        (paint g asteroid))
      (paint g @ship))
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
      (do-action! ship (actions (.getKeyCode e)) false))
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
