(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.scenes.world :as world]
)
(fn [phys-world presets]
  (let [
      human-preset (presets :human)
      player (world/load-human human-preset [0 0 0] [0 0 1])
    ]
    [player]
  )
)