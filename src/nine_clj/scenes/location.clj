(ns nine-clj.scenes.location
  [:require
    [nine-clj.generic :as generic]
    [nine-clj.datum :as dat]
  ]
)

(declare location-render-loop)

(defn location-setup [dev res player level-preset world-state]
  (let [
      { :keys [preset pos look color side] } player
      pause-menu (res :location-pause-menu-setup)
      level (assoc level-preset
        :presets preset
        :update-state generic/generic-update
        :update-phys phys/update-world
        :next-state dat/next-game-state
        :spawn (fn [phys-world preset] (dat/load-char phys-world preset pos look color side 0))
      )
    ]
    (generic/generic-setup dev res generic/generic-loop generic/render-loop pause-menu)
  )
)

(defn location-render-loop [dev res state]
  (generic/generic-render-loop dev res state)
  (-> res :world :model graph/model)
)
