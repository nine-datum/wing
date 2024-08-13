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
  )
  (:import
    [nine.lwjgl
      LWJGL_Window
    ]
    [nine.function
      RefreshStatus
      UpdateRefreshStatus
      Condition
    ]
    [nine.math
      FloatFunc
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
    [nine.geometry.collada
      ColladaBasicAnimationParser
      ColladaBasicSkinParser
      ColladaBasicMaterialParser
    ]
  )
)

(def storage (FileStorage.))

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
      (update-status proc-refresh-status)
      (graph/reset-matrix-stack)
      (reset! window-width w)
      (reset! window-height h)
      (swap! state (partial loop dev))
      ((dev :mouse) :update)
      ((dev :keyboard) :update)
    )
  )
)

(defn windowStart [setup loop]
  (proxy [WindowStartAction] []
    (start [id]
      (let [dev { :gl (graph/new-gl) :keyboard (input/keyboard id) :mouse (input/mouse id proc-refresh-status) }]
        ((dev :mouse) :update)
        ((dev :keyboard) :update)
        (reset! state (setup dev))
        (org.lwjgl.glfw.GLFW/glfwSwapInterval 0)
        (windowLoop id dev loop)
      )
    )
  )
)

(defn test-setup [dev]
  (let
    [
      gl (dev :gl)
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      
      presets (dat/load-presets gl storage diffuse-shader skin-shader)
      
      scene (graph/load-model graphics "res/datum/scene/arena.dae")
      image (graph/load-image gl storage "res/images/example.png")
      image-shader (graph/load-shader gl storage "res/shaders/image_vertex.glsl" "res/shaders/image_fragment.glsl")
      phys-world (phys/dynamics-world)
      level-geom (geom/read-geom storage "res/datum/scene/arena.dae")
      level-geom (mapv :vertex level-geom)
      level-shape (mapv phys/geom-shape level-geom)
      level-body (mapv #(phys/add-rigid-body phys-world % [0 0 0] [0 0 0] 0) level-shape)

      players (mapv
        (fn [preset i]
          (let [dir (apply math/x0y (math/clock-xy (* i (/ Math/PI 2))))]
            (dat/load-char phys-world preset (mapv * dir (repeat -10)) dir get-time)
          )
        )
        presets (range (count presets))
      )
      player (first players)
      non-players (rest players)
    ]
    {
      :phys-world phys-world
      :player player
      :non-players non-players
      :scene scene
      :image image
      :image-shader image-shader
      :campos (mapv * (player :pos) (repeat 1.1))
      :camrot [0 0 0]
      :movement [0 0 0]
      :time (get-time)
    }
  )
)

(defn test-loop [dev state]
  (let [
      time (get-time)
      dt (- time (state :time))
      state (do
        (phys/update-world (state :phys-world) dt)
        (dat/update-game-state dev state)
        (assoc state :time time)
      )
      state (dat/next-game-state dev state)
      {:keys [
          player
          non-players
          scene
          campos
          camrot
          image
          image-shader
        ]
      } state
    ]

    (graph/world-light [0 -1 0])

    (graph/projection (math/perspective (width) (height) (math/radians 60) 0.01 100))
    (graph/camera (math/first-person-camera campos camrot))
    
    (graph/model scene)

    (dat/render-char player)
    (doseq [n non-players] (dat/render-char n))

    (graph/image image image-shader -1 -0.5 0.5 0.5)

    state
  )
)

(defn window [w h setup loop params]
  (.run (LWJGL_Window.) w h
    (windowStart setup loop)
  )
)

(defn -main [& args]
  (window 800 600 test-setup test-loop {})
)
