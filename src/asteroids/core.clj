(ns asteroids.core
  (:import (java.awt Graphics Graphics2D Color Dimension Polygon)
           (javax.swing JPanel JFrame Timer JOptionPane JFileChooser)
           (java.awt.event ActionListener KeyListener)
           )
  (:require [asteroids.common :as c])
  (:use clojure.java.io))

; ----------------------------------------------------------
; game
; ----------------------------------------------------------

(defn game-panel [frame]
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]                                     ; <label id="code.game-panel.paintComponent"/>
      (proxy-super paintComponent g)
      (c/paint-all g))
    (actionPerformed [e]
      (c/time-tick!)
      (when (c/lose? @c/game-state)
        (c/reset-game! false)
        (JOptionPane/showMessageDialog frame "You lose!"))
      (when (c/win? @c/game-state)
        (c/reset-game! true)
        (JOptionPane/showMessageDialog frame "You win!"))
      (.repaint this))
    (keyPressed [e]
      (c/do-action! (c/actions (char (.getKeyCode e)))))
    (keyReleased [e]
      (c/do-action! (c/actions (char (.getKeyCode e))) false))
    (keyTyped [e])
    (getPreferredSize []
      (let [[width height] (c/resize-game! 1000 800)]
        (Dimension. width height)))))

(defn game []
  (let [frame (JFrame. "Asteroids")
        panel (game-panel frame)
        timer (Timer. c/turn-millis panel)]
    (doto panel                                             ; <label id="code.game.panel"/>
      (.setFocusable true)
      (.addKeyListener panel)
      (.setBackground (Color. 0 0 0)))
    (doto frame                                             ; <label id="code.game.frame"/>
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer)                                          ; <label id="code.game.timer"/>
    (def ship (:ship @c/game-state))
    (def timer timer)
    (def frame frame)
    {:ship ship}))                                          ; <label id="code.game.return"/>
