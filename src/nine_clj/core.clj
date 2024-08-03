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
      LocalTime
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

(def time-obj (LocalTime.))
(defn get-time [] (.value time-obj))

(def delta-time-obj (.delta time-obj proc-refresh-status))
(defn get-delta-time [] (.value delta-time-obj))

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
      (let [dev { :keyboard (input/keyboard id) :mouse (input/mouse id proc-refresh-status) }]
        ((dev :mouse) :update)
        ((dev :keyboard) :update)
        (reset! state (setup dev))
        (windowLoop id dev loop)
      )
    )
  )
)

(defn test-setup [dev]
  (let
    [
      gl (graph/new-gl)
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      
      presets ((comp cycle mapv)
        (partial dat/load-preset gl storage diffuse-shader skin-shader)
        [ :ninja :mage :archer :fighter ]
      )
      
      scene (graph/load-model graphics "res/datum/scene/arena.dae")
      image (graph/load-image gl storage "res/images/example.png")
      image-shader (graph/load-shader gl storage "res/shaders/image_vertex.glsl" "res/shaders/image_fragment.glsl")
      phys-world (phys/dynamics-world)
      level-geom (geom/read-geom storage "res/datum/scene/arena.dae")
      level-geom (mapv :vertex level-geom)
      level-shape (mapv phys/geom-shape level-geom)
      level-body (mapv #(phys/add-rigid-body phys-world % [0 0 0] [0 0 0] 0) level-shape)
      body (phys/capsule phys-world [0 4 0] [0 0 0] 0.25 1.5 1)
      body (phys/set-rotation-enabled body false)
    ]
    {
      :phys-world phys-world
      :presets presets
      :scene scene
      :image image
      :image-shader image-shader
      :body body
      :campos [0 0 -1]
      :look [0 0 1]
    }
  )
)

(defn test-loop [dev state]
  (phys/update-world (state :phys-world) (get-delta-time))
  (let [
      state (dat/update-player-state dev state)
      {:keys [
          phys-world
          presets
          scene
          body
          campos
          camrot
          image
          image-shader
          look
          movement
        ]
      } state
      [mov-x mov-y mov-z] (mapv (partial * 6) movement)
      [vel-x vel-y vel-z] (phys/get-velocity body)
      kb (dev :keyboard)
      vel-y (if (kb "v" :down) 10 vel-y)
      [look-x look-y look-z] look
    ]
    (graph/world-light [0 -1 0])

    (phys/set-velocity body [mov-x vel-y mov-z])

    (graph/projection (math/perspective (width) (height) (math/radians 60) 0.01 100))
    (graph/camera (math/first-person-camera campos camrot))
    
    (graph/model scene)

    (graph/push-matrix)
    (graph/apply-matrix (math/rotation 0 (math/clock look-x look-z) 0))
    (graph/apply-matrix (math/mat4f (phys/get-matrix body)))
    (graph/apply-matrix (math/translation 0 -1 0))
    (dat/render-preset (first presets) (if (zero? (+ (abs mov-x) (abs mov-z))) "idle" "walk") (get-time))
    (graph/pop-matrix)
    
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
