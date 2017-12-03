(ns ld40.core.desktop-launcher
  (:require [ld40.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. ld40-game "ld40" 840 640)
  (Keyboard/enableRepeatEvents true))
