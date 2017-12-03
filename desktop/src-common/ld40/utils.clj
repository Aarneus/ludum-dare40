(ns ld40.utils
  (:require
    [play-clj.core :refer :all]
    [play-clj.g2d :refer :all]))


(def screen-width 640)
(def screen-height 640)

(def token-size 128)
(def particle-size 64)
(def half-token-size (/ token-size 2))
(def font-width 16)
(def font-height 32)
(def map-width 5)
(def map-height 5)
(def tween-speed 0.2)

(def particle-life 20)

(def message-width 512)
(def message-height 256)
(def message-offset-x (/ (- screen-width message-width) 2))
(def message-offset-y (+ token-size (/ (- screen-height message-height) 2)))

(def textures (atom {}))
(def sounds (atom {}))
(def musics (atom {}))
(def id (atom 0))
(def level (atom 0))

(def tween-keywords
  (list :tween-x :x
        :tween-y :y
        :tween-angle :angle))

(defn radian-to-degree [x]
  (/ (* 180 x) Math/PI))

(defn floor [i]
  (-> i (Math/floor) (int)))

(defn square [x]
  (* x x))

(defn euclidean-distance [xs ys]
  (Math/sqrt (->> (map - ys xs) (map square) (reduce +))))

(defn get-entity-by-id [entities id]
  (find-first (fn [e] (= (:id e) id)) entities))

(defn get-next-id! []
  "Creates a new id and returns it"
  (swap! id + 1))

(defn in?
  [coll element]
  (some #(= element %) coll))

(defn is? [entity]
  (fn [e] (= (:id e) (:id entity))))

(defn in-tile? [tile-x tile-y]
  (fn [e] (and (= tile-x (:tile-x e)) (= tile-y (:tile-y e)))))

(defn not-player? [entity]
  (and (:token? entity) (not (:player? entity))))


(defn offset-to-cube [col row]
  "Translates offset coordinates to cube coordinates"
  (let [cx (- col (/ (- row (bit-and row 1)) 2))
        cz row
        cy (- 0 cx cz)]
    [cx cy cz]))

(defn cube-distance [a b]
  "Calculates the hex distance in cube coordinates"
  (let [ax (nth a 0) ay (nth a 1) az (nth a 2)
        bx (nth b 0) by (nth b 1) bz (nth b 2)]
    (max (Math/abs (- ax bx))
         (Math/abs (- ay by))
         (Math/abs (- az bz)))))

(defn get-token-distance [a b]
  "Returns the tile distance between to tokens"
  (let [ac (offset-to-cube (:tile-x a) (:tile-y a))
        bc (offset-to-cube (:tile-x b) (:tile-y b))]
    (cube-distance ac bc)))


(defn -load-texture! [filename width height]
  "Loads a plain texture into the cache"
  (let [sheet (texture filename)
        tiles (texture! sheet :split width height)]
    (do (swap! textures assoc (str filename width "x" height) tiles)
      tiles)))

(defn get-texture! [filename width height]
  "Returns the wanted texture and loads it if it's not cached"
  (let [id (str filename width "x" height)]
    (if (some? (get @textures id))
      (get @textures id)
      (do
        (-load-texture! filename width height)
        (get @textures id)))))

(defn get-sound! [filename]
  "Returns the wanted sound file and loads it if not cached"
  (if (some? (get @sounds filename))
    (get @sounds filename)
    (do
      (swap! sounds assoc filename (sound filename))
      (get @sounds filename))))

(defn play-sound! [filename]
  "Plays the given sound file"
  (sound! (get-sound! filename) :play))

(defn load-and-play-music! [filename silent?]
  "Plays the given sound file as a music channel"
  (swap! musics assoc filename (music filename :play :is-looping :set-looping true :set-volume (if silent? 0.0 1.0))))

(defn set-music-volume! [filename volume]
  (let [song (get @musics filename)]
    (when (some? song) (music! song :set-volume volume))))


(defn create-sprite! [filename x y z width height frame-x frame-y]
  "Creates the given sprite"
  (-> (get-texture! filename width height)
      (aget frame-x frame-y)
      (texture)
      (assoc :x x :y y :z z :id (get-next-id!)
        :filename filename :width width :height height
        :angle 0
        :tween-speed tween-speed)))

(defn set-frame [entity frame-x frame-y]
  (let [filename (:filename entity)
        width (:width entity)
        height (:height entity)
        object (:object
                 (-> (get-texture! filename width height)
                     (aget frame-x frame-y)
                     (texture)))]
  (assoc entity :object object)))


(defn get-screen-x [tile-x tile-y]
  "Returns the screen x coordinate for the given tile"
  (+ (* tile-x token-size) (* (mod tile-y 2) (/ token-size 2))))

(defn get-screen-y [tile-x tile-y]
  "Returns the screen y coordinate for the given tile"
  (* tile-y token-size))

(defn get-tile-y [screen-x screen-y]
  "Returns the tile coordinate y for the given screen coordinates"
  (-> screen-y (/ token-size) (floor)))

(defn get-tile-x [screen-x screen-y]
  "Returns the tile coordinate x for the given screen coordinates"
  (-> screen-x (- (* (/ token-size 2) (mod (get-tile-y screen-x screen-y) 2))) (/ token-size) (floor)))


(defn get-tokens-at [entities x y]
  "Returns the entity in screen coordinates x and y"
  (let [tile-y (get-tile-y x y)
        tile-x (get-tile-x x y)]
    (filter (fn [entity]
                  (and (:token? entity)
                       (= (:tile-x entity) tile-x)
                       (= (:tile-y entity) tile-y)))
                  entities)))


(defn update-entities [entities condition effect]
  "Applies the effect to the entities that fulfill the condition"
  (map
    (fn [entity] (if (condition entity) (effect entity) entity))
    entities))

(defn clear-messages [entities]
  "Deletes all messages"
  (filter (fn [e] (not (:message? e))) entities))


(defn show-message [entities message]
  "Sets the given message image up"
  (-> entities
      (clear-messages)
      (conj (assoc (create-sprite!
                     message message-offset-x message-offset-y 6
                     512 256 0 0)
              :message? true))))




