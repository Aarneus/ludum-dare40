(ns ld40.control
  (:require
    [ld40.utils :as u]
    [ld40.data :as d]
    [ld40.process :as p]
    [ld40.world :as w]
    [play-clj.core :refer :all]
    [play-clj.g2d :refer :all]))


; entities x y frame tween-angle tween-x tween-y

(defn move-entity [entities entity tile-x tile-y]
  (if (or (nil? tile-x) (nil? tile-y)) entities
    (let [tween-x (u/get-screen-x tile-x tile-y)
          tween-y (u/get-screen-y tile-x tile-y)]
      (do (u/play-sound! (rand-nth ["settler-move-1.wav" "settler-move-2.wav"]))
        (-> entities
            (d/create-particle! (:x entity) (:y entity) 0 180 tween-x tween-y)
            (u/update-entities
              #(and (:moves? %) ((u/in-tile? (:tile-x entity) (:tile-y entity)) %))
              #(assoc % :tile-x tile-x :tile-y tile-y :tween-x tween-x :tween-y tween-y)))))))


; Settlers
(defn get-best-location [entities settler]
  (let [neighbors (d/get-empty-neighbors entities (:tile-x settler) (:tile-y settler))
        best (if (empty? neighbors) nil (first (d/sort-by-cities entities neighbors)))]
  best))

(defn settle [entities settler]
  (do (u/play-sound! "city-spawn.wav")
    (-> entities
        (d/create-particle! (+ ( * 3 u/font-width) (:x settler)) (+ u/particle-size (:y settler)) 0 180 (:x settler) (:y settler))
        (d/create-particle! (+ u/particle-size (:x settler)) (:y settler) 0 180 (:x settler) (:y settler))
        (d/create-particle! (+ (* 3 u/font-width) (:x settler)) (+ u/font-height (:y settler)) 0 180 (:x settler) (:y settler))
        (d/create-particle! (:x settler) (:y settler) 0 180 (:x settler) (:y settler))
        (d/despawn (:tile-x settler) (:tile-y settler))
        (d/create-city! (:tile-x settler) (:tile-y settler) 1 1 1))))

(defn wander [entities settler]
  (let [dest (get-best-location entities settler)]
    (-> entities
        (u/update-entities (u/is? settler) #(assoc % :wandering (dec (:wandering %))))
        (move-entity settler (first dest) (second dest)))))

(defn settler-ai [entities settler]
  (let [best-location (get-best-location entities settler)
        best-value (if (some? best-location) (nth best-location 2) 999)
        value-here (count (d/get-neighboring-cities entities (:tile-x settler) (:tile-y settler)))
        no-city-here? (not (d/is-at? entities :city? (:tile-x settler) (:tile-y settler)))
        best-here? (< value-here best-value)
        almost-best-here? (<= value-here best-value)
        wandered? (>= 0 (:wandering settler))]
    (if (and no-city-here? (or best-here? (and almost-best-here? wandered?)))
      (settle entities settler)
      (wander entities settler))))


(defn handle-settlers [entities]
  (loop [e entities s (into [] (filter :settler? entities))]
    (if (empty? s) e (recur (settler-ai e (peek s)) (pop s)))))


; Cities
(defn adjust-city-stat [entities city stat amount]
  (-> entities
      (u/update-entities (u/is? city) #(assoc % stat (max 0 (+ amount (stat city)))))))

(defn split-city [entities city]
  (if (or (d/is-at? entities :moves? (:tile-x city) (:tile-y city)) (< (:population city) d/population-limit)) entities
    (-> entities
        (u/update-entities (u/is? city) #(assoc % :population (- (:population city) d/population-split)))
        (d/create-settler! (:tile-x city) (:tile-y city)))))

(defn city-planning [entities city]
  (let [tile-x (:tile-x city)
        tile-y (:tile-y city)
        broke? (>= 1 (:wealth city))
        secure? (> (:military city) (:wealth city))
        at-wealth-cap? (>= (:wealth city) (+ (:military city) (:population city)))
        neighbors (d/get-neighbors entities tile-x tile-y)
        enemies (map #(d/get-at entities :city? (first %) (second %)) neighbors)
        most-powerful-enemy (when-let [highest (last (sort-by :military enemies))] (:military highest))]
    (if (or broke? (and secure? (or (nil? most-powerful-enemy) (< most-powerful-enemy (:military city)))))
      (adjust-city-stat entities city :wealth 1)
      (-> entities
          (adjust-city-stat city :military 1)
          (adjust-city-stat city :wealth -1)))))

(defn city-ai [entities city]
  (-> entities
      (adjust-city-stat city :population 1)
      (city-planning city)
      (split-city city)))

(defn handle-cities [entities]
  (loop [e entities c (into [] (filter :city? entities))]
    (if (empty? c) e (recur (city-ai e (peek c)) (pop c)))))


(defn bomb [entities tile-x tile-y]
  (let [targets (into [] (filter #(and (:city? %) (<= (u/get-token-distance % {:tile-x tile-x :tile-y tile-y}) (w/get-bomb-area entities))) entities))]
    (u/play-sound! "boom.wav")
    (loop [e entities t targets]
      (if (empty? t) e
        (recur
          (let [city (peek t)
                x (:x city)
                y (:y city)
                damage (- (w/get-bomb-damage entities))]
            (-> e
                (adjust-city-stat city :military damage)
                (adjust-city-stat city :population damage)
                (adjust-city-stat city :wealth damage)
                (p/remove-dead-city city)
                (d/create-particle! (+ (* 3 u/font-width) x) (+ u/particle-size y) 1 180 x y)
                (d/create-particle! (+ u/particle-size x) y 1 180 x y)
                (d/create-particle! (+ (* 3 u/font-width) x) (+ u/font-height y) 1 180 x y)
                (d/create-particle! (+ (* 1 u/font-width) x) (+ u/font-width y) 1 180 x y)
                (d/create-particle! (+ u/font-width x) (+ (* 3 u/font-width) y) 1 180 x y)
                (d/create-particle! x y 1 180 x y)))
          (pop t))))))



; Game turns
(defn next-turn [entities]
  (-> entities
      (u/update-entities :turn #(assoc % :turn (inc (:turn %))))
      (handle-cities)
      (handle-settlers)))

(defn click [entities x y left?]
  (u/play-sound! "click.wav")
  (if left?
    (next-turn entities)
    (next-turn (bomb entities (u/get-tile-x x y) (u/get-tile-y x y)))))
