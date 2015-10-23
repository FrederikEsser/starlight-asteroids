(ns asteroids.interop
  (:import (java.awt Graphics Graphics2D Color Dimension Polygon)
           (java.io File)
           (javax.sound.sampled AudioSystem FloatControl$Type))
  (:use clojure.java.io))

(defn new-color
  ([r g b]
   (Color. r g b))
  ([r g b a]
   (Color. r g b (int a)))
  ([color alpha]
   (new-color (.getRed color) (.getGreen color) (.getBlue color) alpha)))

(defn get-alpha [color]
  (.getAlpha color))

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

