(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.scenes.world :as world]
  '[nine-clj.math :as math]
)
(fn [phys-world presets]
  (let [
      human-preset (presets :human)
      player (world/load-human human-preset [-50 0 -50] (math/normalize[-1 0 -1]))
    ]
    [player]
  )
)