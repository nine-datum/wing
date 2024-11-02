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
      player (world/load-unit horse-preset rider-preset [0 1 0 1] [0 0 0] (math/normalize[-1 0 -1]))
    ]
    [player]
  )
)
