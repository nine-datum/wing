(require
  '[nine-clj.scenes.arena :as arena]
)
(fn [phys-world presets]
  (arena/arena-spawn phys-world presets [0 1 0 1] [1 0 0 1] :green :red (repeat 10 :archer) (repeat 10 :ninja))
)
