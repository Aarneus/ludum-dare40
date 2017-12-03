(ns ld40.world
  (:require [ld40.utils :as u]
            [ld40.data :as d]
            [clojure.data :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.core :refer :all]))

(def generate-map)


(defn main-menu []
  "Jumps to the main menu"
   (-> [(assoc
          (u/create-sprite! "map.png" 0 0 0 840 640 0 0)
          :main? true :turn 0)]
       (generate-map)))


(defn generate-map [entities]
  (-> entities
      (d/create-settler! 2 2)
  ))


(defn get-turn [entities]
  (:turn (find-first :turn entities)))

(defn get-bomb-damage [entities]
  (/ (d/get-total-stat entities :military) 5))

(defn get-bomb-area [entities]
  (/ (d/get-total-stat entities :military) 40))





(defn update-menu [entities]
  (let [purged (into [] (filter #(nil? (:menu-number? %)) entities))]
    (-> purged
        (concat (d/create-number-menu! 784 480 (get-turn entities)))

        (concat (d/create-number-menu! 784 416 (d/get-total-stat entities :military)))
        (concat (d/create-number-menu! 784 384 (d/get-total-stat entities :population)))
        (concat (d/create-number-menu! 784 352 (d/get-total-stat entities :wealth)))

        (concat (d/create-number-menu! 784 288 (get-bomb-area entities)))
        (concat (d/create-number-menu! 784 256 (get-bomb-damage entities)))


        )))
