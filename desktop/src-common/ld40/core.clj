(ns ld40.core
  (:require [ld40.utils :as u]
            [ld40.world :as w]
            [ld40.data :as d]
            [ld40.control :as c]
            [ld40.process :as p]
            [play-clj.core :refer :all]
            [play-clj.ui :refer :all]))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (graphics! :set-resizable false)
    (graphics! :set-title "Uncivilized - Ludum Dare 40")
    (update! screen :renderer (stage))
    (w/main-menu))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (sort-by :z)
         (p/remove-dead-particles)
         (p/update-numbers)
         (p/update-cities)
         (p/update-music-volumes!)
         (p/animate)
         (w/update-menu)
         (render! screen)))

  :on-touch-down
  (fn [screen entities]
    (-> entities
        (c/click (game :x) (game :y) (= (:button screen) (button-code :left)))))


  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :f12)) (do (screenshot! "screenshot.png") entities)
      (= (:key screen) (key-code :escape)) (w/main-menu))))


(defgame ld40-game
  :on-create
  (fn [this]
    (u/load-and-play-music! "population.ogg" false)
    (u/load-and-play-music! "military.ogg" false)
    (u/load-and-play-music! "wealth.ogg" false)
    (u/play-sound! "game-over.wav")
    (set-screen! this main-screen)))


