(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.scenes.world :as world]
  '[nine-clj.math :as math]
)
(fn [phys-world presets]
  (let [
      horse-preset (presets :horse)
      player (world/load-unit horse-preset [-50 0 -50] (math/normalize[-1 0 -1]))
    ]
    [player]
  )
)
