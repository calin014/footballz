(ns footballz.shared.game)

(def field-width 1000)
(def field-height 600)

(defn vect-scl-prod [[x y] scalar] [(* scalar x) (* scalar y)])
(defn vect-dot-prod [[x1 y1] [x2 y2]] (+ (* x1 x2) (* y1 y2)))
(defn vect-div [[x y] scalar] [(/ x scalar) (/ y scalar)])
(defn vect-diff [[x1 y1] [x2 y2]] [(- x2 x1) (- y2 y1)])
(defn vect-sum [[x1 y1] [x2 y2]] [(+ x2 x1) (+ y2 y1)])
(defn vect-magnitude [[x y]] (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))
(defn vect-dist [a b] (vect-magnitude (vect-diff a b)))
(defn vect-tangent [[x y]] [(- y) x])
(defn vect-unit [v] (let [magn (vect-magnitude v)]
                      (if (zero? magn) [0 0] (vect-div v magn))))
(defn vect-cap-magnitude [v cap]
  (let [unit (vect-unit v)
        capped-magnitude (min (vect-magnitude v) cap)]
    (vect-scl-prod unit capped-magnitude)))

(defn vect-aprox-if-close-enough [v approx delta]
  (let [unit (vect-unit v)
        magnitude (vect-magnitude v)]
    (if (< (Math/abs (- magnitude approx)) delta)
      (vect-scl-prod unit approx)
      v)))

(defn entity-after-update [delta-time entity]
  (let [MAX-SPEED 0.5
        FRICTION-DECEL 0.0005
        with-friction (fn [acc vel]
                        (let [decel (vect-scl-prod (vect-unit vel) (- FRICTION-DECEL))] ;;friction as decel
                          (vect-sum acc decel)))
        {:keys [position velocity acceleration]} entity
        real-acceleration (with-friction acceleration velocity)
        new-velocity (-> real-acceleration
                         (vect-scl-prod delta-time)
                         (vect-sum velocity)
                         (vect-cap-magnitude MAX-SPEED)
                         (vect-aprox-if-close-enough 0 0.01)) ;;stop it
        new-position (-> new-velocity
                         (vect-scl-prod delta-time)
                         (vect-sum position))
        temp (if (or (not= new-velocity velocity) (not= new-position position)) (println [real-acceleration new-velocity new-position]))]
    (assoc entity :position new-position :old-position position :velocity new-velocity)))




(defn pos-vel-after-potential-bounds-collision [pos vel radius lower-bound upper-bound]
  (let [vel-adjustment -0.8]
    (cond
      (< (- pos radius) lower-bound) [radius (* vel vel-adjustment)]
      (> (+ pos radius) upper-bound) [(- upper-bound radius) (* vel vel-adjustment)]
      :else [pos vel])))

(defn entities-after-bound-collisions [entity]
  (let [{[x y]         :position
         r             :radius
         [vel-x vel-y] :velocity} entity
        [x vel-x] (pos-vel-after-potential-bounds-collision x vel-x r 0 field-width)
        [y vel-y] (pos-vel-after-potential-bounds-collision y vel-y r 0 field-height)]
    (assoc entity :position [x y] :velocity [vel-x vel-y])))

(defn entities-after-position-updates [delta-time entities]
  (map (partial entity-after-update delta-time) entities))

(defn after-collision [entity1 entity2]
  (let [{pos1 :position m1 :mass v1 :velocity} entity1
        {pos2 :position m2 :mass v2 :velocity} entity2
        normal (vect-diff pos1 pos2)
        unit-normal (vect-unit normal)
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
    [(assoc entity1 :position (:old-position entity1) :velocity (vect-scl-prod v1-final 0.8)) ;;lose some E
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

(defn entities-after-inter-collisions [entities]
  (->> entities
       (concat (updated-colliding-entities entities))
       (distinct-by :id)))                                  ;;TODO: recur until no collision found

(defn entities-after-collisions [delta-time entities]
  (->> entities
       (entities-after-inter-collisions)
       (map entities-after-bound-collisions)))

(defn update-entities [entities delta-time]
  (->> (vals entities)
       (entities-after-position-updates delta-time)
       (entities-after-collisions delta-time)
       (map #(dissoc % :old-position))
       (map #(vector (:id %) %))
       (into (hash-map))))



;; API

(defn new-game
  "Returns the game initial state."
  []
  {:entities {:ball {
                     :id           :ball,
                     :type         :ball
                     :acceleration [0 0]
                     :velocity     [0 0]
                     :position     [(/ field-width 2) (/ field-height 2)]
                     :radius       10
                     :mass         3
                     :color        "white"}}})


(defn join
  "Adds a player to the game entity list."
  [game player-id player-name]
  (update game :entities assoc player-id {
                                          :id           player-id,
                                          :type         :player
                                          :name         (:name player-name),
                                          :acceleration [0 0]
                                          :velocity     [0 0]
                                          :position     [(+ 10 (rand-int (- field-width 10)))
                                                         (+ 10 (rand-int (- field-height 10)))]
                                          :radius       20
                                          :mass         10
                                          :color        "red"}))

(defn leave
  "Removes plyer with given id from the entity list."
  [game player-id]
  (update game :entities dissoc player-id))


(defn handle-player-command
  "Modifies players attributes according to command."
  [game player-id command]
  (let [ACC 0.0018
        INIT-SPD 0.03]
    (case command
      [:add :accel-x] (-> game
                          (update-in [:entities player-id :acceleration 0] (constantly ACC))
                          (update-in [:entities player-id :velocity 0] (constantly INIT-SPD)))
      [:add :decel-x] (-> game
                          (update-in [:entities player-id :acceleration 0] (constantly (- ACC)))
                          (update-in [:entities player-id :velocity 0] (constantly (- INIT-SPD))))
      [:add :accel-y] (-> game
                          (update-in [:entities player-id :acceleration 1] (constantly ACC))
                          (update-in [:entities player-id :velocity 1] (constantly INIT-SPD)))
      [:add :decel-y] (-> game
                          (update-in [:entities player-id :acceleration 1] (constantly (- ACC)))
                          (update-in [:entities player-id :velocity 1] (constantly (- INIT-SPD))))
      [:remove :accel-x] (update-in game [:entities player-id :acceleration 0] (constantly 0))
      [:remove :decel-x] (update-in game [:entities player-id :acceleration 0] (constantly 0))
      [:remove :accel-y] (update-in game [:entities player-id :acceleration 1] (constantly 0))
      [:remove :decel-y] (update-in game [:entities player-id :acceleration 1] (constantly 0)))))

(defn update-game
  "Given a game state, update it, by delta-time."
  [game delta-time]                                         ;;in ms
  (update game :entities update-entities delta-time))
