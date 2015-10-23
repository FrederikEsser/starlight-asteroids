(ns asteroids.interop
  (:import (java.awt Graphics Graphics2D Color Dimension Polygon)
           (java.io File)
           (javax.sound.sampled AudioSystem FloatControl$Type))
  (:use clojure.java.io))

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

; ----------------------------------------------------------
; gui
; ----------------------------------------------------------

(defn- java-color [{:keys [r g b a]}]
  (Color. r g b (int a)))

(defn draw-polygon [graphics polygon color]
  (let [jpolygon (new Polygon)]
    (doseq [[x y] polygon] (. jpolygon (addPoint x y)))
    (doto graphics
      (.setColor (java-color color))
      (.drawPolygon jpolygon)))
  nil)

(defn fill-polygon [graphics polygon color]
  (let [jpolygon (new Polygon)]
    (doseq [[x y] polygon] (. jpolygon (addPoint x y)))
    (doto graphics
      (.setColor (java-color color))
      (.fillPolygon jpolygon)))
  nil)

(defn draw-circle [graphics [[x y] [x1 y1]] color]
  (doto graphics
    (.setColor (java-color color))
    (.drawOval x y (- x1 x) (- y1 y)))
  nil)

(defn fill-circle [graphics [[x y] [x1 y1]] color]
  (doto graphics
    (.setColor (java-color color))
    (.fillOval x y (- x1 x) (- y1 y)))
  nil)

