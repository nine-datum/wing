(ns nine-clj.core
  (:gen-class)
  (:require
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.phys :as phys]
    [nine-clj.input :as input]
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
      ((dev :mouse) :update)
      ((dev :keyboard) :update)
      (graph/reset-matrix-stack)
      (reset! window-width w)
      (reset! window-height h)
      (swap! state (partial loop dev))
    )
  )
)

(defn windowStart [setup loop]
  (proxy [WindowStartAction] []
    (start [id]
      (let [dev { :keyboard (input/keyboard id) :mouse (input/mouse id proc-refresh-status) }]
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
      load-offset-animated-model (fn [file & offset-geom-names]
        (graph/load-animated-model
          (graph/load-graphics gl storage diffuse-shader skin-shader
            (graph/geom-offset-parser (partial contains? (apply hash-set offset-geom-names)) [0 0 1])
            (ColladaBasicSkinParser.)
            (ColladaBasicAnimationParser.)
            (ColladaBasicMaterialParser.)
          )
          file
        )
      )
      datum-model-fn (fn [name offset-geom]
        [
          (load-offset-animated-model (format "res/datum/%s.dae" name) offset-geom)
          (graph/load-anim-clj storage (partial = "JOINT") (format "res/datum/anims/%s/attack.clj" name) (format "res/datum/%s.dae" name))
          (graph/load-anim-clj storage (partial = "NODE") (format "res/datum/anims/%s/attack.clj" name) (format "res/datum/%s.dae" name))
        ]
      )
      presets {
        :archer ["archer" "Cube_001-mesh"]
        :mage ["mage" "Cube_002-mesh"]
        :fighter ["fighter" "Cube_002-mesh"]
        :ninja ["ninja" "Cube_003-mesh"]
      }
      load-preset (fn [name]
        (apply datum-model-fn (presets name))
      )
      [model clj-anim clj-obj-anim] (load-preset :ninja)
      scene (graph/load-model graphics "res/models/Scenes/Mountains.dae")
      image (graph/load-image gl storage "res/images/example.png")
      image-shader (graph/load-shader gl storage "res/shaders/image_vertex.glsl" "res/shaders/image_fragment.glsl")
      font (text/default-font 12)
      text-shader image-shader
      textfn (graph/live-text gl text-shader)
      body (do (phys/plane [0 1 0] 0) (phys/box [0 10 0] [0 0 0] [1 1 1] 1))
      body (phys/set-rotation-enabled body false)
    ]
    {
      :font font
      :textfn textfn
      :model model
      :anim (graph/instance clj-anim (. RefreshStatus always))
      :obj-anim (graph/instance clj-obj-anim (. RefreshStatus always))
      :scene scene
      :image image
      :image-shader image-shader
      :body body
      :camrot [0 0 0]
    }
  )
)

(defn test-loop [dev state]
  (phys/update (get-delta-time))

  (let [
      {:keys [
          scene
          model
          anim
          body
          camrot
          obj-anim
          image
          image-shader
          textfn
          font
        ]
      } state
      { :keys [keyboard mouse] } dev
      [camx camy camz] camrot
      [mousex mousey] (mapv (partial * 0.01) (mouse :delta))
      camrot [(- camx mousey) (+ camy mousex) camz]
      campos (phys/get-position body)
      [wasd-x wasd-y] (input/wasd keyboard)
      cammat (apply math/rotation camrot)
      cam-fwd (math/get-column-3 cammat 2)
      cam-right (math/get-column-3 cammat 0)
      cam-fwd (mapv (partial * wasd-y) cam-fwd)
      cam-right (mapv (partial * wasd-x) cam-right)
      [mov-x mov-y mov-z] (mapv (comp (partial * 3) +) cam-fwd cam-right)
      [vel-x vel-y vel-z] (phys/get-velocity body)
      state (assoc state :camrot camrot)
    ]

    (phys/set-velocity body [mov-x vel-y mov-z])

    (graph/projection (math/perspective (width) (height) (math/radians 60) 0.01 100))
    (graph/camera (math/orbital-camera (apply math/vec3f campos) (apply math/vec3f camrot) 5))
    (graph/model scene)
    
    (graph/push-matrix)
    (graph/apply-matrix (math/mat4f (phys/get-matrix body)))
    (graph/apply-matrix (math/translation 0 -1 0))
    (graph/animated-model model (graph/animate anim (get-time)) (graph/animate obj-anim (get-time)))
    (graph/pop-matrix)
    
    (graph/image image image-shader -1 -0.5 0.5 0.5)

    (when (keyboard "c" :down)
      (doseq [i (range 0 40)]
        (textfn "Hello, text!" font -0.5 (- 1 (* i 0.05)) 0.5 0.05)
      )
    )
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
