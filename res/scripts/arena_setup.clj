(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.graph :as graph]
  '[nine-clj.geom :as geom]
  '[nine-clj.gui :as gui]
  '[nine-clj.input :as input]
  '[nine-clj.phys :as phys]
  '[nine-clj.prof :as prof]
  '[nine-clj.math :as math]
)
(use 'nine-clj.core)

(defn arena-setup [dev]
  (let
    [
      { :keys [gl storage mouse] } dev
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      
      presets (dat/load-presets gl storage diffuse-shader skin-shader)
      
      scene (graph/load-model graphics "res/datum/scene/arena.dae")
      gui-asset (gui/gui-asset gl storage (input/viewport-mouse mouse width height))
      phys-world (phys/dynamics-world)
      level-geom (geom/read-geom storage "res/datum/scene/arena.dae")
      level-geom (mapv :vertex level-geom)
      level-shape (mapv phys/geom-shape level-geom)
      level-body (mapv #(phys/add-rigid-body phys-world % [0 0 0] [0 0 0] 0) level-shape)

      players ((read-script "res/scripts/arena_spawn.clj") phys-world presets)
      player (first players)
      non-players (rest players)
      [campos camrot] (dat/player-cam player)
    ]
    {
      :phys-world phys-world
      :player player
      :non-players non-players
      :items ()
      :scene scene
      :gui-asset gui-asset
      :campos campos
      :camrot camrot
      :time (get-time)
    }
  )
)

(defn arena-loop [dev state]
  (prof/reset)
  (prof/profile :main-loop (let [
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

      (mapv (fn [t i] (gui/button gui-asset t -0.2 (-> i (+ 0.4) (* -0.2)) 0.4 0.15))
        ["Начать игру" "Настройки" "Выйти"]
        (range)
      )
    ))

    state
  ))
)
(comp #(assoc % :loop arena-loop) arena-setup)