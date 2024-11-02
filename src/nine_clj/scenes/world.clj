(ns nine-clj.scenes.world
  [:require
    [nine-clj.scenes.generic :as generic]
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
    [nine-clj.input :as input]
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

(defn load-unit-preset [dev diffuse-shader skin-shader geom-file idle-file walk-file]
  (let
    [
      { :keys [gl storage ] } dev
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
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
    :human (load-unit-preset dev diffuse-shader skin-shader
      "res/world/human/human-idle.dae"
      "res/world/human/human-idle.dae"
      "res/world/human/human-walk.dae"
    )
    :horse (load-unit-preset dev diffuse-shader skin-shader
      "res/world/horse/horse_run.dae"
      "res/world/horse/horse_idle.dae"
      "res/world/horse/horse_run.dae"
    )
  }
)

(defn load-unit [preset pos look]
  {
    :pos pos
    :look look
    :model (preset :model)
    :anims (preset :anims)
    :anim :walk
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
  (generic/generic-loop dev res state)
)

(defn update-world-state [dev state] state)

(defn next-world-state [dev state]
  (let [
      { :keys [keyboard] } dev
      { :keys [player non-players campos camrot time delta-time] } state
      camrot-xy (get state :camrot-xy [0 0])
      camdist (get state :camdist 8)
      camdist (->
        (cond
          (input/key-down keyboard \q) 1
          (input/key-down keyboard \e) -1
          :else 0
        )
        (* delta-time 30)
        (+ camdist)
        (max 4)
        (min 30)
      )
      [arr-x arr-y] (input/arrows keyboard)
      [cx cy] (mapv + camrot-xy [(-> arr-y - (* delta-time 2)) (* arr-x delta-time 2)])
      cx (->> Math/PI (* 1/4) (min cx) (max 0))
      camrot-xy [cx cy]
      [cdir-x cdir-z] (math/clock-xy cy)
      [cdir-y _] (math/clock-xy cx)
      camdir [(- cdir-x) cdir-y (- cdir-z)]
      camrot [cx cy 0]
      campiv (->> player :pos (mapv + [0 3 0]))
      campos (->> camdir math/normalize (mapv * (repeat camdist)) (mapv + campiv))
    ]
    (assoc state
      :campos campos
      :camrot camrot
      :camrot-xy camrot-xy
      :camdist camdist
    )
  )
)
