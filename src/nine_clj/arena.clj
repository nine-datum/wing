(ns nine-clj.arena
  [:require
    [nine-clj.datum :as dat]
    [nine-clj.core :as core]
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.gui :as gui]
    [nine-clj.input :as input]
    [nine-clj.phys :as phys]
    [nine-clj.prof :as prof]
    [nine-clj.math :as math]
    [nine-clj.scripting :as scripting]
  ]
)

(declare arena-loop)

(defn arena-setup [dev res]
  (let
    [
      { :keys [arena arena-shape char-presets gui-asset] } res
      phys-world (phys/dynamics-world)
      arena-body (mapv #(phys/add-rigid-body phys-world % [0 0 0] [0 0 0] 0) arena-shape)

      players ((scripting/read-file "res/scripts/arena_spawn.clj") phys-world char-presets)
      player (first players)
      non-players (rest players)
      [campos camrot] (dat/player-cam player)
    ]
    {
      :phys-world phys-world
      :player player
      :non-players non-players
      :items ()
      :scene arena
      :gui-asset gui-asset
      :campos campos
      :camrot camrot
      :time (-> dev :get-time list eval)
      :loop arena-loop
    }
  )
)

(defn arena-loop [dev state]
  (prof/reset)
  (prof/profile :main-loop (let [
      { :keys [get-time width height] } dev
      time (get-time)
      dt (- time (state :time))
      pdt (min dt 1/10)
      state (do
        (prof/profile :jbullet-update (phys/update-world (state :phys-world) pdt))
        (prof/profile :game-update (dat/update-game-state dev state))
        (assoc state :time time :delta-time pdt)
      )
      state (prof/profile :game-next (dat/next-game-state dev state))
      {:keys [
          player
          non-players
          items
          scene
          campos
          camrot
          gui-asset
        ]
      } state
    ]

    (prof/profile :rendering (do
      (graph/world-light [0 -1 0])

      (graph/projection (math/perspective (width) (height) (math/radians 60) 0.01 1000))
      (graph/camera (math/first-person-camera campos camrot))

      (graph/model scene)

      (doseq [n (concat [player] non-players items)] (dat/render-char n))
    ))

    state
  ))
)