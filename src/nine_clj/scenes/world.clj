(ns nine-clj.scenes.world
  [:require
    [nine-clj.scenes.generic :as generic]
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
  ]
  [:import
    [nine.main TransformedDrawing]
    [nine.opengl Drawing Shader ShaderPlayer]
  ]
)

(declare world-loop)

(defn world-setup [dev res]
  (let [
      { :keys [world-pause-menu-setup] } res
    ]
    (generic/generic-setup dev res world-loop world-pause-menu-setup :world)
  )
)

(defn load-world-model [dev file]
  (let [
      { :keys [gl storage] } dev
      shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/world_fragment.glsl")

      graphics (graph/load-graphics gl storage shader shader)
      tex-names ["sand.png" "rock.png" "grass.png" "snow.png"]
      tex-names (mapv (partial str "res/datum/scene/world/") tex-names)
      textures (mapv #(.texture gl (.open storage %)) tex-names)
      uniforms (mapv
        #(-> shader
          .player
          .uniforms
          (.uniformTexture (str "texture" %) %)
        )
        (range 1 5)
      )
      uniforms-load (proxy [Drawing] [] (draw [] (mapv #(.load %1 %2) uniforms textures)))
      model (graph/load-model graphics file)
      model-geom (model :model)
      geom (proxy [TransformedDrawing] []
        (transform [proj light root mats]
          (.draw (.play (.player shader) uniforms-load))
          (.transform model-geom proj light root mats)
        )
      )
    ]
    (assoc model :model geom)
  )
)

(defn load-human-preset [dev diffuse-shader skin-shader geom-file idle-file walk-file]
  (let
    [
      { :keys [gl storage ] } dev
      graphics (graph/load-graphics gl storage skin-shader diffuse-shader)
      model (graph/load-animated-model graphics geom-file)
      idle-anim (graph/load-anim graphics idle-file)
      idle-obj-anim (graph/load-obj-anim graphics idle-file)
      walk-anim (graph/load-anim graphics walk-file)
      walk-obj-anim (graph/load-obj-anim graphics walk-file)
    ]
    {
      :model model
      :anims { :idle [idle-anim idle-obj-anim] :walk [walk-anim walk-obj-anim] }
    }
  )
)

(defn load-presets [dev diffuse-shader skin-shader]
  {
    :human (load-human-preset dev diffuse-shader skin-shader
      "res/world/human/human-idle.dae"
      "res/world/human/human-idle.dae"
      "res/world/human/human-walk.dae"
    )
  }
)

(defn load-human [preset pos look]
  {
    :pos pos
    :look look
    :model (preset :model)
    :anims (preset :anims)
    :anim :idle
    :render (fn [ch time]
      (let [
          { :keys [pos look] } ch
          [lx ly lz] look
          rot-y (math/clock lx lz)
          anims (ch :anims)
          [anim obj-anim] (->> ch :anim anims (mapv #(graph/animate % time)))
        ]
        (graph/push-matrix)
        (graph/apply-matrix (math/transform pos [0 rot-y 0] [1 1 1]))
        (-> ch :model (graph/animated-model anim obj-anim))
        (graph/pop-matrix)
      )
    )
  }
)

(defn world-loop [dev res state]
  (generic/generic-render-loop dev res state)
  state
)