(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.scenes.world :as world]
  '[nine-clj.math :as math]
)
(fn [phys-world presets]
  (let [
      horse-preset (presets :horse)
      rider-preset (presets :ninja)
      player (world/load-unit phys-world horse-preset rider-preset [0 1 0 1] [0 2 0] [0 0 1])
    ]
    [player]
  )
)
