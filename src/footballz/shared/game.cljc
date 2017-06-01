(ns footballz.shared.game)

(def field-width 1000)
(def field-height 600)


(defn join [game player-id player-name]
  (update game :players assoc player-id {
                                         :id       player-id,
                                         :commands (list),  ;;need order, latest first
                                         :name     (:name player-name),
                                         :velocity [0 0]
                                         :position [(/ field-width 2) (/ field-height 2)]
                                         :radius   20}))


(defn leave [game player-id]
  (update game :players dissoc player-id))

(defn remove-command [coll x]
  (remove #(= x %) coll))

(defn add-command [coll x]
  (conj (remove-command coll x) x))

(defn register-player-command [game player-id [op command]]
  (let [ops {:add add-command, :remove remove-command}]
    (update-in game [:players player-id :commands] (ops op) command)))

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

(defn update-player [delta-time player]
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

(defn dist [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))))

(defn scl-prod [[x y] scalar] [(* scalar x) (* scalar y)])

(defn entity-pair-after-potential-collision [entity1 entity2]
  (let [{pos1     :position
         old-pos1 :old-position
         r1       :radius
         vel1     :velocity} entity1
        {pos2     :position
         old-pos2 :old-position
         r2       :radius
         vel2     :velocity} entity2
        collision-dist (- (dist pos1 pos2) (+ r1 r2))]
    (if (<= collision-dist 0)
      [(assoc entity1 :position old-pos1 :velocity (scl-prod vel2 -0.8))
       (assoc entity2 :position old-pos2 :velocity (scl-prod vel1 -0.8))]
      [entity1 entity2])))


(defn apply-bound-collisions [entity]
  (let [{[x y]         :position
         r             :radius
         [vel-x vel-y] :velocity} entity
        [x vel-x] (pos-vel-after-potential-bounds-collision x vel-x r 0 field-width)
        [y vel-y] (pos-vel-after-potential-bounds-collision y vel-y r 0 field-height)]

    (assoc entity :position [x y] :velocity [vel-x vel-y])))


(defn update-all-players [players delta-time]
  (zipmap (keys players) (->> (vals players)
                              (map (partial update-player delta-time))
                              (map apply-bound-collisions)
                              (map #(dissoc % :old-position)))))


(defn update-game [game delta-time]                         ;;in ms
  (update game :players update-all-players delta-time))
