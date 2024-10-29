(ns nine-clj.scenes.world
  [:require
    [nine-clj.scenes.generic :as generic]
  ]
)

(defn world-setup [dev res]
  (let [
      { :keys [world-pause-menu-setup] } res
    ]
    (generic/generic-setup dev res generic/generic-loop world-pause-menu-setup :world)
  )
)