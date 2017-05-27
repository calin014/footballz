(ns footballz.shared.game)

(defn join [game player-id player-name]
  (update game :players assoc player-id {
                                         :id       player-id,
                                         :commands (list),  ;;need order, latest first
                                         :name     (:name player-name),
                                         :speed-x  0,
                                         :speed-y  0,
                                         :x        (rand-int 100),
                                         :y        (rand-int 100)}))

(defn leave [game player-id]
  (update game :players dissoc player-id))

(defn remove-command [coll x]
  (remove #(= x %) coll))

(defn add-command [coll x]
  (conj (remove-command coll x) x))

(defn register-player-command [game player-id [op command]]
  (let [ops {:add add-command, :remove remove-command}]
    (update-in game [:players player-id :commands] (ops op) command)))

(defn calc-speed [transform old-speed delta-time]
  (let [max-speed 0.5
        acceleration 0.0003
        deceleration 0.00015]
    (if transform
      (if (< (- max-speed) old-speed max-speed)
        (transform old-speed (* acceleration delta-time))
        old-speed)
      (let [increment (* deceleration delta-time)]
        (cond
          (> old-speed increment) (- old-speed increment)
          (< old-speed (- increment)) (+ old-speed increment)
          :else 0)))))

(defn update-player [delta-time player]
  (let [{:keys [commands x y speed-x speed-y]} player
        transform-mapping {:inc-speed-x +
                           :dec-speed-x -
                           :inc-speed-y +
                           :dec-speed-y -}
        speed-x-transform (transform-mapping (some #{:inc-speed-x :dec-speed-x} commands))
        speed-y-transform (transform-mapping (some #{:inc-speed-y :dec-speed-y} commands))
        speed-x (calc-speed speed-x-transform speed-x delta-time)
        speed-y (calc-speed speed-y-transform speed-y delta-time)
        x (+ x (* speed-x delta-time))
        y (+ y (* speed-y delta-time))]
    (assoc player :x x :y y :speed-x speed-x :speed-y speed-y)))

(defn update-all-players [players delta-time]
  (zipmap (keys players) (map (partial update-player delta-time) (vals players))))

(defn update-game [game delta-time]                         ;;in ms
  (update game :players update-all-players delta-time))
