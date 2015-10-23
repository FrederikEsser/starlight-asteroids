(ns ^:figwheel-always asteroids.core
  (:require
    [asteroids.common :as c]
    [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
    [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {}))

(defn repaint [graphics]
  (let [canvas (.-canvas graphics)
        width (.-width canvas)
        height (.-height canvas)]
    (.clearRect graphics 0 0 width height))
  (c/paint-all graphics))

(defn time-loop [_]
  (when (:running @app-state)
    (c/time-tick!)
    (when (c/lose? @c/game-state) (js/alert "You lose!") (c/reset-game! false))
    (when (c/win? @c/game-state) (js/alert "You win!") (c/reset-game! true))
    (repaint (:context @app-state))
    (go
      (<! (timeout c/turn-millis))
      (.requestAnimationFrame js/window time-loop))))

(defn start []
  (swap! app-state assoc :running true)
  (.requestAnimationFrame
    js/window
    (fn [time]
      (time-loop time))))

(defn stop []
  (swap! app-state assoc :running false))

(defn toggle-running []
  (if (:running @app-state)
    (stop)
    (start)))

(defn on-key-down [e]
  (or
    (c/do-action! (c/actions (char (.-keyCode e))))
    (condp = (char (.-keyCode e))
      \P (toggle-running) #_(.preventDefault e)
      :default nil)))

(defn on-key-up [e]
  (c/do-action! (c/actions (char (.-keyCode e))) false))

(defn init []
  (let [canvas (.createElement js/document "canvas")
        context (.getContext canvas "2d")]
    (set! (.-width canvas) (* (inc c/width) c/point-size))
    (set! (.-height canvas) (* (inc c/height) c/point-size))
    (set! (.. canvas -style -background) "rgb(0,0,0)")
    (set! (.-onkeydown js/document) on-key-down)
    (set! (.-onkeyup js/document) on-key-up)
    (.appendChild (.-body js/document) canvas)
    (swap! app-state assoc
           :context context))
  (start))

(when-not (:context @app-state)
  (init))


