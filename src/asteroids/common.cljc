(ns asteroids.common
  (:require [asteroids.matrix-math :as m]
            [asteroids.interop :as i]))

; ----------------------------------------------------------
; functional model
; ----------------------------------------------------------

(def width 120)
(def height 60)
(def point-size 12)
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
                                    :color        (i/new-color 250 50 10)
                                    :speed        2.5
                                    :acceleration 0
                                    :damage       3
                                    :cooldown     300
                                    :explosion    {:size      3
                                                   :color     (i/new-color 220 180 180)
                                                   :life-time 400}}
                       :fire       {:size           (/ ship-size 2.5)
                                    :shape          [[-1.0 -1.0] [1.0 -1.0] [0.0 1.0]]
                                    :color          #(i/new-color (random-int 230 256) (random-int 60 220) (random-int 0 50))
                                    :spread         0.2
                                    :speed          #(random -0.8 -0.5)
                                    :acceleration   0
                                    :rotation-speed #(random -5 5)
                                    :cooldown       30
                                    :life-time      400}
                       :missile    {:size         (/ ship-size 3)
                                    :shape        [[0.0 1.0] [0.5 -1.0] [0.5 -3.5] [1.0 -4.5] [0.5 -4.5] [0.25 -4.25]
                                                   [-0.25 -4.25] [-0.5 -4.5] [-1.0 -4.5] [-0.5 -3.5] [-0.5 -1.0]]
                                    :color        (i/new-color 10 50 250)
                                    :speed        0
                                    :acceleration 3
                                    :damage       15
                                    :cooldown     8000
                                    :explosion    {:size      30
                                                   :color     (i/new-color 220 220 250)
                                                   :life-time 200}}
                       :bomb       {:size         (/ ship-size 3)
                                    :shape        [[-1.0 -1.0] [0.0 -1.5] [1.0 -1.0] [1.1 1.0] [0.0 1.5] [-1.0 1.0]]
                                    :color        (i/new-color 10 250 50)
                                    :speed        0.8
                                    :acceleration 0
                                    :damage       1
                                    :cooldown     5000
                                    :explosion    {:size      20
                                                   :color     (i/new-color 180 220 180)
                                                   :life-time 160
                                                   :damage    3}}
                       })

(def actions {
              \A {:rotation (- ship-rotation-speed)}
              \D {:rotation ship-rotation-speed}
              \W {:acceleration ship-acceleration}
              \J {:fire-weapon :launcher}
              \L {:fire-weapon :bomber}
              \K {:fire-weapon :machinegun}
              })


(def sqrt (memoize #(Math/sqrt %)))

(defn get-transformation-matrix [{:keys [size position direction]} scale]
  (let [[x y] position]
    (->> (m/scale size)
         (m/rotate-mat direction)
         (m/transpose-mat x y)
         (m/scale-mat scale))))

(defn polygon-to-screen [pol mat]
  (let [transform (fn [v] (->> (conj (vec v) 1)
                               (m/mat*vec mat)
                               (take 2)
                               (m/round)))]
    (map transform pol)))

(defn create-weapon [ammunition-type]
  {:ammunition-type ammunition-type
   :cooldown        0
   :shooting?       false})

(defn create-ship []
  {:type           :ship

   :size           ship-size
   :shape          [[-0.143 1.0] [0.143 1.0] [0.429 0.429] [1.0 0.143] [1.0 -1.0] [0.429 -0.714]
                    [0.143 -1.0] [-0.143 -1.0] [-0.429 -0.714] [-1.0 -1.0] [-1.0 0.143] [-0.429 0.429]]
   :color          (i/new-color 160 160 150)
   :paint-method   i/fill-polygon

   :cockpit-shape  [[-0.143 0.714] [0.143 0.714] [0.286 -0.143] [-0.286 -0.143]]
   :cockpit-color  (i/new-color 0 0 80)

   :weapons        {:engine     (create-weapon :fire)
                    :machinegun (create-weapon :projectile)
                    :launcher   (create-weapon :missile)
                    :bomber     (create-weapon :bomb)}
   :shots          []

   :position       [(/ width 2) (/ height 2)]
   :velocity       [0 0]
   :acceleration   0

   :direction      0
   :rotation-speed 0
   :rotation       #{}

   :life           ship-life
   :life-colors    {0 (i/new-color 255 0 0)
                    1 (i/new-color 255 80 75)
                    2 (i/new-color 250 160 75)
                    3 (i/new-color 160 160 150)}
   :collidable?    false                                    ; is set to ship-damage at first movement / shooting
   })

(defn get-expr-value
  ([expr default]
   (cond
     (nil? expr) default
     (fn? expr) (expr)
     :else expr))
  ([expr] (get-expr-value expr nil)))

(defn create-shot [ammunition-type {ship-pos :position ship-size :size ship-dir :direction ship-velocity :velocity}]
  (let [{:keys [spread size shape color acceleration direction rotation-speed damage life-time]} (get ammunition-types ammunition-type)
        move-dir (if spread (+ ship-dir (random (- spread) spread)) ship-dir)
        dir-vec (m/rotate-vec move-dir [0 1])
        position (m/vec+vec ship-pos (m/num*vec ship-size dir-vec))
        speed (get-expr-value (-> ammunition-types ammunition-type :speed))
        velocity (m/vec+vec ship-velocity (m/num*vec speed dir-vec))]
    {:type           ammunition-type

     :size           (get-expr-value size)
     :shape          (get-expr-value shape)
     :color          (get-expr-value color)
     :paint-method   i/fill-polygon

     :position       position
     :velocity       velocity
     :acceleration   (get-expr-value acceleration 0)

     :direction      (get-expr-value direction move-dir)
     :rotation-speed (get-expr-value rotation-speed 0)

     :collidable?    (get-expr-value damage)

     :life-time      (get-expr-value life-time)}))

(defn create-explosion [position {:keys [size color life-time damage] :as explosion}]
  (if explosion
    {:type           :explosion

     :size           size
     :shape          [[-1 -1] [1 1]]

     :color          color
     :paint-method   i/fill-circle

     :position       position
     :velocity       [0 0]
     :acceleration   0

     :direction      0
     :rotation-speed 0

     :collidable?    damage

     :life-time      life-time
     }
    nil))

(defn create-asteroid
  ([size pos velocity]
   (let [size (vary asteroid-size-variance size)
         direction (rand (* 2 (Math/PI)))
         dir-vec (m/rotate-vec direction [0 1])
         velocity (m/vec+vec velocity (m/num*vec (rand asteroid-max-speed) dir-vec))
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
      :color          (i/new-color 250 240 (if debris? 150 20))
      :paint-method   i/draw-polygon

      :position       position
      :velocity       velocity
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
  ([num size] (repeatedly num #(create-asteroid size)))
  ([num size pos velocity] (repeatedly num #(create-asteroid size pos velocity))))

(defn out-of-bounds? [{[x y] :position size :size}]
  (or
    (< x (- size))
    (> x (+ width size))
    (< y (- size))
    (> y (+ height size))
    ))

(defn collide? [{size1 :size [x1 y1] :position} {size2 :size [x2 y2] :position}]
  (let [delta-x (- x1 x2)
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
                          (if (type (set (keys ammunition-types))) :shot type)))

(defmethod handle-damage :ship [{:keys [damage-taken life] :as ship}]
  (when damage-taken (i/play-sound (if (<= life damage-taken) "ship-destroyed" "ship-hit")))
  (if damage-taken
    (assoc ship :life (- life damage-taken)
                :damage-taken nil)
    ship))

(defmethod handle-damage :shot [{:keys [type damage-taken position] :as shot}]
  (when damage-taken (i/play-sound (str (name type) "-hit")))
  (if damage-taken
    (create-explosion position (-> ammunition-types type :explosion))
    shot))

(defmethod handle-damage :asteroid [{:keys [damage-taken size position velocity] :as asteroid}]
  (if damage-taken
    (create-asteroids damage-taken (/ size (sqrt damage-taken)) position velocity)
    asteroid))

(defmethod handle-damage :default [object]
  object)

(defn win? [state]
  (empty? (:asteroids state)))

(defn lose? [state]
  (<= (get-in state [:ship :life]) 0))

; ----------------------------------------------------------------------
; Actions
; ----------------------------------------------------------------------

(defn reset-game [level]
  {:ship      (create-ship)
   :asteroids (create-asteroids level asteroid-start-size)
   :level     level})

(defn fire-weapon [{:keys [shots] :as ship}
                   [weapon-type {:keys [ammunition-type cooldown shooting?]}]]
  (let [cooldown (if (> cooldown turn-millis) (- cooldown turn-millis) 0)
        shoots? (and shooting? (= cooldown 0))
        cooldown (if shoots? (-> ammunition-types ammunition-type :cooldown) cooldown)
        shots (if shoots? (conj shots (create-shot ammunition-type ship)) shots)]
    (when shoots? (i/play-sound (str (name ammunition-type) "-fired")))
    (-> ship
        (assoc-in [:weapons weapon-type :cooldown] cooldown)
        (assoc :shots shots))))

(defn ship-shoot [{{:keys [weapons] :as ship} :ship :as state}]
  (assoc state :ship (reduce fire-weapon ship weapons)))

(defn basic-move
  ([{:keys [direction rotation-speed velocity acceleration position] :as object} wrap?]
   (let [direction (+ direction (* rotation-speed turn-millis 0.001))
         velocity (m/vec+vec velocity (m/rotate-vec direction [0 (* acceleration turn-millis 0.001)]))
         [x y] (m/vec+vec velocity position)]
     (assoc object :direction direction
                   :velocity velocity
                   :position (if wrap? [(mod x width) (mod y height)] [x y]))))
  ([object] (basic-move object true)))

(defmulti move :type)

(defmethod move :ship [{shots :shots :as ship}]
  (let [ship (basic-move ship)
        shots (->> shots
                   (remove out-of-bounds?)
                   (map move)
                   (remove nil?))]
    (assoc ship :shots shots)))

(defmethod move :default [object]
  (basic-move object false))

(defmethod move :fire [{:keys [life-time color] :as fire}]
  (if (< life-time turn-millis)
    nil
    (let [fire (basic-move fire false)
          alpha (* (i/get-alpha color) (- 1 (/ turn-millis life-time)))
          color (i/new-color color alpha)
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
          alpha (* (i/get-alpha color) (- 1 (/ turn-millis life-time)))
          color (i/new-color color alpha)
          life-time (- life-time turn-millis)]
      (assoc explosion :size size
                       :life-time life-time
                       :color color))))

(defn move-many [coll]
  (->> coll
       (map move)
       (remove nil?)))

(defn resolve-collision [{{shots :shots :as ship} :ship asteroids :asteroids :as state}]
  (let [[ship asteroids] (check-collision ship asteroids)
        [shots asteroids] (check-collision shots asteroids)
        ship (handle-damage ship)
        shots (->> shots
                   (map handle-damage)
                   flatten
                   (remove nil?))
        asteroids (->> asteroids
                       (map handle-damage)
                       flatten
                       (remove nil?))]
    (assoc state :ship (assoc ship :shots shots)
                 :asteroids asteroids)))

(defn update-positions [{:keys [ship asteroids] :as state}]
  (assoc state :ship (move ship)
               :asteroids (move-many asteroids)))

(defn time-tick [state]
  (-> state
      ship-shoot
      update-positions
      resolve-collision))

(defn toggle-ship-shooting [{{:keys [weapons] :as ship} :ship :as state} weapon-type activate?]
  (if (contains? weapons weapon-type)
    (assoc state :ship (-> ship
                           (assoc-in [:weapons weapon-type :shooting?] activate?)
                           (assoc :collidable? ship-damage)))
    state))

(defn accelerate-ship [{ship :ship :as state} acceleration activate?]
  (-> state
      (assoc :ship (assoc ship :acceleration (if activate? acceleration 0)
                               :collidable? ship-damage))
      (toggle-ship-shooting :engine activate?)))

(defn set-ship-rotation [{{rotation-set :rotation :as ship} :ship :as state} rotation activate?]
  (let [rotation (if activate?
                   (conj rotation-set rotation)
                   (disj rotation-set rotation))]
    (assoc state :ship (assoc ship :rotation rotation
                                   :rotation-speed (reduce + rotation)))))

; ----------------------------------------------------------
; mutable model
; ----------------------------------------------------------

(defonce game-state (atom (reset-game asteroids-start-number)))

(defn reset-game! [increase-level?]
  (let [level (:level @game-state)]
    (reset! game-state (reset-game (if increase-level? (inc level) level)))))

(defn time-tick! []
  (swap! game-state time-tick))

(defn do-action!
  ([{:keys [fire-weapon acceleration rotation]} activate?]
   (when fire-weapon (swap! game-state toggle-ship-shooting fire-weapon activate?))
   (when acceleration (swap! game-state accelerate-ship acceleration activate?))
   (when rotation (swap! game-state set-ship-rotation rotation activate?)))
  ([action]
   (do-action! action true)))

; ----------------------------------------------------------
; gui
; ----------------------------------------------------------

(defmulti paint (fn [_ {type :type}] type))

(defmethod paint :ship [g {:keys [shape life-colors life cockpit-shape cockpit-color shots paint-method] :as ship}]
  (doseq [shot shots]
    (paint g shot))
  (let [transformation (get-transformation-matrix ship point-size)
        polygon (polygon-to-screen shape transformation)
        color (get life-colors life)
        cockpit-polygon (polygon-to-screen cockpit-shape transformation)]
    (paint-method g polygon color)
    (paint-method g cockpit-polygon cockpit-color)))

(defmethod paint :default [g {:keys [shape color paint-method] :as object}]
  (let [transformation (get-transformation-matrix object point-size)
        polygon (polygon-to-screen shape transformation)]
    (paint-method g polygon color)))

(defn paint-all [g]
  (doseq [asteroid (:asteroids @game-state)]
    (paint g asteroid))
  (paint g (:ship @game-state)))

