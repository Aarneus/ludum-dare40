(ns ld40.process
  (:require
    [ld40.utils :as u]
    [ld40.data :as d]
    [play-clj.core :refer :all]
    [play-clj.g2d :refer :all]))

(defn filter-vector [predicate? coll]
  (into [] (filter predicate? coll)))


(defn remove-dead-particles [entities]
  (map #(when (or (not (:particle? %)) (< 0 (:life %))) %) entities))

(defn update-music-volume! [song total]
  (u/set-music-volume! song (min 1.0 (/  (Math/pow total 2) (Math/pow d/max-audio-stat 2)))))


(defn update-music-volumes! [entities]
  "Updates the music volumes according to stat totals"
  (let [total-military (d/get-total-stat entities :military)
        total-population (d/get-total-stat entities :population)
        total-wealth (d/get-total-stat entities :wealth)]
    (update-music-volume! "military.wav" total-military)
    (update-music-volume! "population.wav" total-population)
    (update-music-volume! "wealth.wav" total-wealth)
    entities))


(defn update-numbers [entities]
  "Updates the numbers of all cities"
  (let [purged (map #(if (or (:plaque? %) (:number? %)) nil %) entities)]
    (loop [e purged c (filter-vector :city? purged)]
      (if (empty? c) e
        (let [city (peek c)]
          (recur (d/create-numbers! e
                                    (:tile-x city) (:tile-y city)
                                    (:military city) (:population city) (:wealth city)) (pop c)))))))


(defn update-cities [entities]
  "Updates the buildings of all cities"
  (loop [e entities c (filter-vector :city? entities)]
    (if (empty? c) e
      (let [city (peek c)]
        (recur (d/update-buildings! e (:tile-x city) (:tile-y city)
                                 (:military city) (:population city) (:wealth city)) (pop c))))))


(defn remove-dead-city [entities city]
  (if (or (>= 0 (:military city)) (>= 0 (:population city)) (>= 0 (:wealth city)))
    (do (u/play-sound! "city-despawn.wav")
        (-> entities
          (d/despawn (:tile-x city) (:tile-y city))))
    entities))

(defn remove-dead-cities [entities]
  (loop [e entities c (filter-vector :city? entities)]
    (if (empty? c) e
      (let [city (peek c)]
        (recur (remove-dead-city e city) (pop c))))))


(defn tween-token [entity tween word]
  "Animates the entities with a basic tween"
  (let [t (tween entity)
        w (word entity)]
  (if (some? t)
    (assoc entity word (+ w (* (- t w) (:tween-speed entity))))
    entity)))


(defn animate [entities]
  "Animates all entities including particles and numbers"
  (->> entities

       ; Particles
       (map #(if (not (:particle? %)) % (assoc % :life (dec (:life %)))))


       ; Tweens
       (map (fn [entity]
              (loop [w u/tween-keywords e entity]
                (if (empty? w) e
                  (recur (pop (pop w)) (tween-token e (peek w) (peek (pop w))))))))))
