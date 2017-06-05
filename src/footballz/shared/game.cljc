(ns footballz.shared.game)

(def field-width 1000)
(def field-height 600)

(defn new-game
  "Returns the game initial state."
  []
  {:entities {:ball {
                     :id       :ball,
                     :type     :ball
                     :commands (list),                      ;;TODO: do we need this
                     :velocity [0 0]
                     :position [(/ field-width 2) (/ field-height 2)]
                     :radius   10
                     :mass     3
                     :color    "white"}}})


(defn join
  "Adds a player to the game entity list."
  [game player-id player-name]
  (update game :entities assoc player-id {
                                          :id       player-id,
                                          :type     :player
                                          :commands (list), ;;need order, latest first
                                          :name     (:name player-name),
                                          :velocity [0 0]
                                          :position [(+ 10 (rand-int (- field-width 10))) (+ 10 (rand-int (- field-height 10)))]
                                          :radius   20
                                          :mass     10
                                          :color    "red"}))


(defn leave
  "Removes plyer with given id from the entity list."
  [game player-id]
  (update game :entities dissoc player-id))

(defn remove-command [coll x]
  (remove #(= x %) coll))

(defn add-command [coll x]
  (conj (remove-command coll x) x))

(defn register-player-command [game player-id [op command]]
  (let [ops {:add add-command, :remove remove-command}]
    (update-in game [:entities player-id :commands] (ops op) command)))

(defn apply-acceleration [transform old-vel delta-time]
  (let [max-speed 0.5
        acceleration 0.0003
        idle-deceleration 0.00015]
    (if transform
      (if (< (- max-speed) old-vel max-speed)
        (transform old-vel (* acceleration delta-time))
        old-vel)
      (let [increment (* idle-deceleration delta-time)]     ;;no transform means idle decel
        (cond
          (> old-vel increment) (- old-vel increment)
          (< old-vel (- increment)) (+ old-vel increment)
          :else 0)))))

(defn update-entity-position [delta-time player]
  (let [{commands      :commands
         [x y :as pos] :position
         [vel-x vel-y] :velocity} player
        transform-mapping {:accel-x +
                           :decel-x -
                           :accel-y +
                           :decel-y -}
        vel-x-transform (transform-mapping (some #{:accel-x :decel-x} commands))
        vel-y-transform (transform-mapping (some #{:accel-y :decel-y} commands))
        vel-x (apply-acceleration vel-x-transform vel-x delta-time)
        vel-y (apply-acceleration vel-y-transform vel-y delta-time)
        x (+ x (* vel-x delta-time))
        y (+ y (* vel-y delta-time))]
    (assoc player :position [x y] :old-position pos :velocity [vel-x vel-y])))


(defn pos-vel-after-potential-bounds-collision [pos vel radius lower-bound upper-bound]
  (let [vel-adjustment -0.8]
    (cond
      (< (- pos radius) lower-bound) [radius (* vel vel-adjustment)]
      (> (+ pos radius) upper-bound) [(- upper-bound radius) (* vel vel-adjustment)]
      :else [pos vel])))


;;TODO: use vector funs in velocity calc also
(defn vect-scl-prod [[x y] scalar] [(* scalar x) (* scalar y)])
(defn vect-dot-prod [[x1 y1] [x2 y2]] (+ (* x1 x2) (* y1 y2)))
(defn vect-div [[x y] scalar] [(/ x scalar) (/ y scalar)])
(defn vect-diff [[x1 y1] [x2 y2]] [(- x2 x1) (- y2 y1)])
(defn vect-sum [[x1 y1] [x2 y2]] [(+ x2 x1) (+ y2 y1)])
(defn vect-magnitude [[x y]] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))
(defn vect-dist [a b] (vect-magnitude (vect-diff a b)))
(defn vect-tangent [[x y]] [(- y) x])

(defn apply-bound-collisions [entity]
  (let [{[x y]         :position
         r             :radius
         [vel-x vel-y] :velocity} entity
        [x vel-x] (pos-vel-after-potential-bounds-collision x vel-x r 0 field-width)
        [y vel-y] (pos-vel-after-potential-bounds-collision y vel-y r 0 field-height)]
    (assoc entity :position [x y] :velocity [vel-x vel-y])))

(defn entities-after-position-updates [delta-time entities]
  (map (partial update-entity-position delta-time) entities))

(defn after-collision [entity1 entity2]
  (let [{pos1 :position m1 :mass v1 :velocity} entity1
        {pos2 :position m2 :mass v2 :velocity} entity2
        normal (vect-diff pos1 pos2)
        unit-normal (vect-div normal (vect-magnitude normal))
        unit-tangent (vect-tangent unit-normal)
        v1n (vect-dot-prod unit-normal v1)
        v1t (vect-dot-prod unit-tangent v1)
        v2n (vect-dot-prod unit-normal v2)
        v2t (vect-dot-prod unit-tangent v2)
        v1t-final v1t
        v2t-final v2t
        v1n-final (/ (+ (* v1n (- m1 m2)) (* 2 m2 v2n)) (+ m1 m2))
        v2n-final (/ (+ (* v2n (- m2 m1)) (* 2 m1 v1n)) (+ m1 m2))
        v1n-final-vect (vect-scl-prod unit-normal v1n-final)
        v1t-final-vect (vect-scl-prod unit-tangent v1t-final)
        v2n-final-vect (vect-scl-prod unit-normal v2n-final)
        v2t-final-vect (vect-scl-prod unit-tangent v2t-final)
        v1-final (vect-sum v1n-final-vect v1t-final-vect)
        v2-final (vect-sum v2n-final-vect v2t-final-vect)]
    [(assoc entity1 :position (:old-position entity1) :velocity (vect-scl-prod v1-final 0.8))
     (assoc entity2 :position (:old-position entity2) :velocity (vect-scl-prod v2-final 0.8))]))

(defn colliding? [{pos1 :position r1 :radius} {pos2 :position r2 :radius}]
  (<= (- (vect-dist pos1 pos2) (+ r1 r2)) 0))

(defn distinct-by [fn coll]
  (map first (vals (group-by fn coll))))

(defn collision-groups [entities]
  (->> (let [n (count entities)]
         (for [i (range n)
               j (range (inc i) n)
               :let [e1 (entities i) e2 (entities j)]]
           (if (colliding? e1 e2) (after-collision e1 e2))))
       (filter some?)))

(defn updated-colliding-entities [entities]
  (->> (collision-groups (vec entities))
       (flatten)))

(defn entities-after-collisions [entities]
  (->> entities
       (concat (updated-colliding-entities entities))       ;;TODO: recur untill no collision found
       (distinct-by :id)))

(defn apply-collision-updates [delta-time entities]
  (->> entities
       (entities-after-collisions)
       (map apply-bound-collisions)))

(defn update-entities [entities delta-time]
  (->> (vals entities)
       (entities-after-position-updates delta-time)
       (apply-collision-updates delta-time)
       (map #(dissoc % :old-position))
       (map #(vector (:id %) %))
       (into (hash-map))))


(defn update-game [game delta-time]                         ;;in ms
  (update game :entities update-entities delta-time))
