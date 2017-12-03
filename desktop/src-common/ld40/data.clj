
(ns ld40.data
  (:require [ld40.utils :as u]
            [play-clj.g2d :refer :all]
            [play-clj.core :refer :all]))


(def population-limit 10)
(def population-split 5)
(def water-tiles [[0 0] [4 0] [3 1] [0 2]])
(def max-audio-stat 100)

(defn out-of-bounds? [tile-x tile-y]
  (or (u/in? water-tiles [tile-x tile-y])
      (< tile-y 0)
      (> tile-y 4)
      (< tile-x 0)
      (> tile-x (if (= (mod tile-y 2) 0) 4 3))))

(defn get-at [entities predicate? tile-x tile-y]
  (find-first #(and (= tile-x (:tile-x %))
                    (= tile-y (:tile-y %))
                    (predicate? %)) entities))

(defn is-at? [entities predicate? tile-x tile-y]
  (some? (get-at entities predicate? tile-x tile-y)))


(defn tile-is-empty? [entities tile-x tile-y]
  (nil? (find-first #(and (= (:tile-x %) tile-x) (= (:tile-y %) tile-y)) entities)))

(defn get-neighbors [entities tile-x tile-y]
  (->> (if (= 0 (mod tile-y 2))
         (shuffle [[-1 0] [-1 1] [0 1] [1 0] [0 -1] [-1 -1]])
         (shuffle [[-1 0] [0 1] [1 1] [1 0] [1 -1] [0 -1]]))
       (map #(vector (+ tile-x (first %)) (+ tile-y (second %))))
       (filter #(not (out-of-bounds? (first %) (second %))))))

(defn get-empty-neighbors [entities tile-x tile-y]
  (->> (get-neighbors entities tile-x tile-y)
       (filter #(tile-is-empty? entities (first %) (second %)))))

(defn get-neighboring-cities [entities tile-x tile-y]
  (->> (get-neighbors entities tile-x tile-y)
       (map #(get-at entities :city? (first %) (second %)))
       (filter some?)
       (into [])))

(defn sort-by-cities [entities tiles]
  (->> tiles
       (map #(hash-map :x (first %) :y (second %) :neighbors (count (get-neighboring-cities entities (first %) (second %)))))
       (sort-by :neighbors)
       (map #(vector (:x %) (:y %) (:neighbors %)))
       (into [])))

(defn get-total-stat [entities stat]
  (->> entities
       (filter :city?)
       (map stat)
       (reduce + 0)))

(defn calculate-number-x [tile-x tile-y index]
  (+ (case index
       0 0
       1 (* u/font-width 3)
       2 (* u/font-width 6))
     (u/get-screen-x tile-x tile-y)))

(defn calculate-number-y [tile-x tile-y index]
  (u/get-screen-y tile-x tile-y))


(defn create-particle! [entities x y frame tween-angle tween-x tween-y]
  (-> (u/create-sprite! "font.png" x y 6 u/particle-size u/particle-size 3 frame)
      (assoc :particle? true
        :tween-x tween-x :tween-y tween-y :tween-angle tween-angle
        :life u/particle-life :original-life u/particle-life :tween-speed 0.01)
      (->> (conj entities))))

(defn create-number-sprite! [tile-x tile-y index frame-y offset plaque?]
  (let [x (+ (calculate-number-x tile-x tile-y index) offset)
        y (calculate-number-y tile-x tile-y index)
        width (if plaque? (* 2 u/font-width) u/font-width)
        frame (if plaque? (/ frame-y 2) frame-y)]
    (u/create-sprite! "font.png" x y 4 width u/font-height 0 (max 0 frame))))

(defn create-number-menu-sprite! [x y value]
  (assoc (u/create-sprite! "font.png" x y 6 u/font-width u/font-height 0 value) :menu-number? true))


(defn get-digits [number]
  (loop [v [] n number]
    (if (= 0 n) (if (not= number 0) v [0])
      (recur (conj v (mod n 10)) (int (/ n 10))))))

(defn create-number-menu! [x y value]
  (loop [s [] sx x n (get-digits value)]
    (if (empty? n) s
      (recur (conj s (create-number-menu-sprite! sx y (peek n))) (+ sx u/font-width) (pop n)))))

(defn create-number-plaque! [tile-x tile-y index]
  (-> (create-number-sprite! tile-x tile-y index (+ 10 (* 2 index)) 0 true)
      (assoc :plaque? true :tile-x tile-x :tile-y tile-y)))

(defn create-number-numbers-sprite! [tile-x tile-y index frame-y offset]
  (assoc (create-number-sprite! tile-x tile-y index frame-y offset false) :number? true :tile-x tile-x :tile-y tile-y))

(defn create-number-numbers! [tile-x tile-y index value]
  (if (< value 10)
    [(create-number-numbers-sprite! tile-x tile-y index value (/ u/font-width 2))]
    [(create-number-numbers-sprite! tile-x tile-y index (/ value 10) 0)
     (create-number-numbers-sprite! tile-x tile-y index (mod value 10) u/font-width)]))

(defn create-numbers! [entities tile-x tile-y military population wealth]
  (concat entities
          [(create-number-plaque! tile-x tile-y 0)
           (create-number-plaque! tile-x tile-y 1)
           (create-number-plaque! tile-x tile-y 2)]
          (create-number-numbers! tile-x tile-y 0 (min 99 military))
          (create-number-numbers! tile-x tile-y 1 (min 99 population))
          (create-number-numbers! tile-x tile-y 2 (min 99 wealth))))

(defn create-token! [tile-x tile-y z frame-x frame-y]
  "Creates a token as a sprite"
  (let [x (u/get-screen-x tile-x tile-y)
        y (u/get-screen-y tile-x tile-y)]
    (-> (u/create-sprite! "tiles.png"  x y z
                          u/token-size u/token-size
                          frame-x frame-y)
        (assoc :token? true
          :tile-x tile-x :tile-y tile-y))))

(defn update-buildings! [entities tile-x tile-y military population wealth]
  (-> entities
      (u/update-entities (u/is? (get-at entities :walls? tile-x tile-y)) #(u/set-frame % 0 (min 4 (/ military 2))))
      (u/update-entities (u/is? (get-at entities :houses? tile-x tile-y)) #(u/set-frame % 1 (min 4 (/ population 2))))
      (u/update-entities (u/is? (get-at entities :tower? tile-x tile-y)) #(u/set-frame % 2 (min 4 (/ wealth 2))))
  ))

(defn create-city! [entities tile-x tile-y military population wealth]
  (-> entities
      (conj (assoc (create-token! tile-x tile-y 1 0 (min 4 (/ military 2))) :city? true :walls? true :military military :population population :wealth wealth))
      (conj (assoc (create-token! tile-x tile-y 2 2 (min 4 (/ wealth 2))) :tower? true))
      (conj (assoc (create-token! tile-x tile-y 3 1 (min 4 (/ population 2))) :houses? true))
      (create-numbers! tile-x tile-y military population wealth)))

(defn create-settler! [entities tile-x tile-y]
  (if (out-of-bounds? tile-x tile-y)
    entities
    (-> entities
        (conj (assoc (create-token! tile-x tile-y 4 3 0) :settler? true :moves? true :wandering (+ 2 (rand-int 2)))))))


(defn despawn [entities tile-x tile-y]
  (filter #(or
             (not= tile-x (:tile-x %))
             (not= tile-y (:tile-y %))) entities))

