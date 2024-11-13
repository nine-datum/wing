(ns nine-clj.io
  [:require
    [nine-clj.datum :as dat]
    [nine-clj.scenes.world :as world]
    [clojure.java.io :refer [make-parents]]
  ]
)

(defn data-to-spawn [data]
  (fn [phys-world presets]
    (let [
        horse (presets :horse)
        ship (presets :ship)
        units (data :units)
      ]
      (mapv (fn [unit]
        (let [
            { :keys [kind pos look color side] } unit
            rider (presets kind)
          ]
          (world/load-horse phys-world horse rider ship color side pos look)
        )
      ) units)
    )
  )
)

(defn unit-to-data [unit]
  (assoc (select-keys unit [:pos :look :side :color :kind])
    :kind (-> unit :preset :name)
  )
)

(defn state-to-data [state]
  (let [
      { :keys [player non-players] } state
      units (as-> player r (cons r non-players) (map unit-to-data r) (list* r))
    ]
    (assoc (select-keys state [ :campos :camrot :camrot-xy :camdist ])
      :units units
    )
  )
)

(defn save-game [state file]
  (make-parents file)
  (->> state state-to-data (spit file))
)

(defn load-game [dev res file]
  (let [
      data (-> file slurp read-string)
      state (select-keys data [:campos :camrot :camrot-xy :camdist])
      spawn (data-to-spawn data)
    ]
    (world/world-load-setup dev res state spawn)
  )
)
