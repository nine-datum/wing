(ns nine-clj.scenes.location
  [:require
    [nine-clj.scenes.generic :as generic]
    [nine-clj.datum :as dat]
    [nine-clj.phys :as phys]
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
  ]
)

(declare location-render-loop)

(defn location-setup [dev res player location world-state]
  (let [
      { :keys [preset pos look color side] } player
      level-preset (location :preset)
      loc-pos (location :pos)
      loc-rot (location :rot)
      loc-entry (location :entry)
      presets [preset]
      pause-menu (res :location-pause-menu-setup)
      make-char (fn [phys-world preset] (dat/load-char phys-world preset loc-entry look color side :idle-pass 0))
      level (assoc level-preset
        :presets presets
        :shapes (concat (level-preset :shapes) (-> res :world :shapes))
        :pos loc-pos
        :rot loc-rot
        :update-state dat/update-game-state
        :update-phys phys/update-world
        :next-state dat/next-game-state
        :spawn (fn [phys-world presets] (mapv make-char (repeat phys-world) presets))
      )
    ]
    (assoc (generic/generic-setup dev res generic/generic-loop location-render-loop pause-menu level)
      :world-state world-state
    )
  )
)

(defn location-render-loop [dev res state]
  (generic/generic-render-loop dev res state)
  (-> res :world :model graph/model)
)
