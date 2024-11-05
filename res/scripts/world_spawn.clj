(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.scenes.world :as world]
  '[nine-clj.math :as math]
)
(fn [phys-world presets]
  (let [
      horse-preset (presets :horse)
      ship-preset (presets :ship)
      rider-presets (mapv presets [:archer :ninja :mage :fighter])
      players (mapv
        #(world/load-horse phys-world horse-preset %1 ship-preset %3 %4 [(+ %2 160) 200 655] [0 0 1])
        rider-presets
        (range)
        [[1/2 1/2 1 1] [0 1 0 1] [0 0 1 1] [1 1 0 1]]
        [:sky :green :blue :yellow]
      )
    ]
    players
  )
)
