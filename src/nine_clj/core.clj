(ns nine-clj.core
  (:gen-class)
  (:require
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.phys :as phys]
    [nine-clj.input :as input]
    [nine-clj.datum :as dat]
    [nine-clj.prof :as prof]
    [nine-clj.gui :as gui]
  )
  (:import
    [nine.lwjgl
      LWJGL_Window
    ]
    [nine.function
      UpdateRefreshStatus
    ]
    [nine.opengl
      WindowStartAction
      WindowLoopAction
    ]
    [nine.io
      FileStorage
    ]
    [nine.game
      Graphics
    ]
    [nine.geometry.procedural
      Geometry
    ]
    [org.lwjgl.glfw
      GLFW
    ]
  )
)

(defn new-status [] (UpdateRefreshStatus.))
(defn update-status [status] (.update status))

(def proc-refresh-status (new-status))

(defn get-time [] (org.lwjgl.glfw.GLFW/glfwGetTime))

(def window-width (atom 0))
(def window-height (atom 0))

(defn width [] @window-width)
(defn height [] @window-height)

(def state (atom {}))

(defn windowLoop [id dev loop]
  (proxy [WindowLoopAction] []
    (call [w h]
      (cond (= (. GLFW GLFW_TRUE) (. GLFW glfwGetWindowAttrib id GLFW/GLFW_FOCUSED))
        (do
          (update-status proc-refresh-status)
          (graph/reset-matrix-stack)
          (reset! window-width w)
          (reset! window-height h)
          (swap! state (partial loop dev))
        )
      )
      ((dev :mouse) :update)
      ((dev :keyboard) :update)
    )
  )
)

(defn window-start [setup loop]
  (proxy [WindowStartAction] []
    (start [id]
      (let [dev { :storage (FileStorage.) :gl (graph/new-gl) :keyboard (input/keyboard id) :mouse (input/mouse id proc-refresh-status) }]
        ((dev :mouse) :update)
        ((dev :keyboard) :update)
        (reset! state (setup dev))
        ;(org.lwjgl.glfw.GLFW/glfwSwapInterval 0) ; fps unlocker
        (windowLoop id dev loop)
      )
    )
  )
)

(defn test-setup [dev]
  (let
    [
      { :keys [gl storage] } dev
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      
      presets (dat/load-presets gl storage diffuse-shader skin-shader)
      
      scene (graph/load-model graphics "res/datum/scene/arena.dae")
      gui-asset (gui/gui-asset gl storage)
      phys-world (phys/dynamics-world)
      level-geom (geom/read-geom storage "res/datum/scene/arena.dae")
      level-geom (mapv :vertex level-geom)
      level-shape (mapv phys/geom-shape level-geom)
      level-body (mapv #(phys/add-rigid-body phys-world % [0 0 0] [0 0 0] 0) level-shape)

      players ((-> "res/scripts/spawn.clj" slurp read-string eval) phys-world presets)
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

(defn test-loop [dev state]
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

      (gui/button gui-asset "Начать игру" -0.2 0 (-> time Math/sin (* 0.2) (+ 0.4)) 0.2)
    ))

    state
  ))
)

(defn window [w h setup loop params]
  (.run (LWJGL_Window.) w h
    (window-start setup loop)
  )
)

(defn -main [& args]
  (window 1200 800 test-setup test-loop {})
)
