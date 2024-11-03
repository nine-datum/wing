(ns nine-clj.scenes.generic
  [:require
    [nine-clj.datum :as dat]
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.gui :as gui]
    [nine-clj.input :as input]
    [nine-clj.phys :as phys]
    [nine-clj.prof :as prof]
    [nine-clj.math :as math]
    [nine-clj.scripting :as scripting]
    [nine-clj.mac :refer [-->]]
  ]
)

(defn generic-setup [dev res loop render-loop pause-menu level]
  (let
    [
      { :keys [gui-asset] } res
      { :keys [presets model shapes spawn update-state update-phys next-state] } (res level)
      phys-world (phys/dynamics-world)
      arena-body (mapv #(phys/add-rigid-body phys-world % [0 0 0] [0 0 0] 0) shapes)

      players (spawn phys-world presets)
      player (first players)
      non-players (rest players)
      [campos camrot] (dat/player-cam player)
    ]
    {
      :phys-world phys-world
      :player player
      :non-players non-players
      :items ()
      :scene model
      :gui-asset gui-asset
      :campos campos
      :camrot camrot
      :movement [0 0 0]
      :time (--> dev :get-time ())
      :loop loop
      :render-loop render-loop
      :update-state update-state
      :update-phys update-phys
      :next-state next-state
      :pause-menu pause-menu
    }
  )
)

(defn generic-render-loop [dev res state]
  (let [
      { :keys [
          player
          non-players
          items
          scene
          campos
          camrot
          gui-asset
          time
        ]
      } state
      { :keys [ width height ] } dev
    ]
    (prof/profile :rendering (do
      (doto (dev :gl)
        (.clearDepth)
        (.clearColor 0.5 0.5 0.7 0)
      )
      (graph/world-light [0 -1 0])

      (graph/projection (math/perspective (width) (height) (math/radians 60) 0.3 20000))
      (graph/camera (math/first-person-camera campos camrot))

      (graph/model scene)

      (doseq [n (concat [player] non-players items)] (dat/render-char n time))
    ))
  )
)

(defn generic-loop [dev res state]
  (let [
      { :keys [get-time width height] } dev
      { :keys [
          next-state
          render-loop
          update-state
          update-phys
        ]
      } state
      real-time (get-time)
      last-real-time (get state :last-real-time real-time)
      dt (- real-time last-real-time)
      pdt (min dt 1/10)
      time (state :time)
      time (+ time pdt)
      state (do
        (prof/profile :update-phys (update-phys (state :phys-world) pdt))
        (prof/profile :update-state (update-state dev state))
        (assoc state :time time :last-real-time real-time :delta-time pdt)
      )
      state (prof/profile :next-state (next-state dev state))
    ]
    (render-loop dev res state)
    (cond (-> dev :keyboard input/escape-up) ((state :pause-menu) dev res state) :else state)
  )
)
