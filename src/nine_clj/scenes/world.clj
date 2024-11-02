(ns nine-clj.scenes.world
  [:require
    [nine-clj.scenes.generic :as generic]
    [nine-clj.graph :as graph]
    [nine-clj.math :as math]
    [clojure.core.matrix :as mat]
    [nine-clj.input :as input]
    [nine-clj.datum :as dat]
    [nine-clj.phys :as phys]
    [nine-clj.mac :refer [-->]]
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
      :anims {
        :idle { :anim idle-anim :obj-anim idle-obj-anim :speed 1 }
        :walk { :anim walk-anim :obj-anim walk-obj-anim :speed 6 }
      }
    }
  )
)

(defn load-presets [dev diffuse-shader skin-shader]
  {
    :horse (load-unit-preset dev diffuse-shader skin-shader
      "res/world/horse/horse_run.dae"
      "res/world/horse/horse_idle.dae"
      "res/world/horse/horse_run.dae"
    )
  }
)

(defn load-unit [phys-world horse-preset rider-preset rider-color pos look]
  {
    :pos pos
    :look look
    :body (doto
      (phys/capsule phys-world pos [0 0 0] 0.5 1.5 100)
      (phys/set-rotation-enabled false)
    )
    :model (horse-preset :model)
    :anims (horse-preset :anims)
    :rider-preset rider-preset
    :rider-materials (dat/load-char-materials rider-preset rider-color)
    :anim :idle
    :update (fn [ch in time]
      (->> in :movement (mapv * (repeat 10)) (phys/move-char (ch :body)))
    )
    :next (fn [ch in time]
      (let [
          { :keys [pos look body] } ch
          anim (-> in :movement mat/length zero? (if :idle :walk))
          look (in :look)
          pos (->> body phys/get-position (mapv + [0 -1 0]))
        ]
        (assoc ch :anim anim :look look :pos pos)
      )
    )
    :render (fn [ch time]
      (let [
          { :keys [pos look rider-preset rider-materials] } ch
          [lx ly lz] look
          rot-y (math/clock lx lz)
          anims (ch :anims)
          { :keys [anim obj-anim speed] } (-> ch :anim anims)
          [anim obj-anim] (mapv #(graph/animate % (* time speed)) [anim obj-anim])
        ]
        (graph/push-matrix)
        (graph/apply-matrix (math/transform pos [0 (+ rot-y Math/PI) 0] [1 1 1]))
        (-> ch :model (graph/animated-model anim obj-anim))
        (graph/apply-matrix-before (.transform anim "rider"))
        (-> Math/PI (/ 2) (math/rotation 0 0) graph/apply-matrix-before)
        (graph/translate 0 0.1 0)
        (dat/render-preset rider-preset rider-materials "armature|riding" time)
        (graph/pop-matrix)
      )
    )
  }
)

(defn world-loop [dev res state]
  (generic/generic-loop dev res state)
)

(defn update-world-state [dev state]
  (let [
      { :keys [keyboard] } dev
      { :keys [player time camrot] } state
      in (dat/move-in (dat/cam-rel-movement keyboard camrot))
    ]
    (--> player :update (player in time))
  )
)

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

      in (dat/ch-move-in player delta-time (dat/cam-rel-movement keyboard camrot))
      player (--> player :next (player in time))
    ]
    (assoc state
      :campos (math/lerpv (state :campos) campos (* delta-time 5))
      :camrot (math/lerpv (state :camrot) camrot (* delta-time 10))
      :camrot-xy camrot-xy
      :camdist camdist
      :player player
    )
  )
)
