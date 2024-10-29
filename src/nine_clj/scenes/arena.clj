(ns nine-clj.scenes.arena
  [:require
    [nine-clj.scenes.generic :as generic]
  ]
)

(declare arena-loop)

(defn arena-setup [dev res]
  (let
    [
      { :keys [arena-pause-menu-setup] } res
    ]
    (generic/generic-setup dev res arena-loop arena-pause-menu-setup :arena)
  )
)

(defn arena-loop [dev res state]
  (generic/generic-loop dev res state)
)