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
(declare world-render-loop)

(defn world-setup [dev res]
  (let [
      { :keys [world-pause-menu-setup] } res
    ]
    (generic/generic-setup dev res world-loop world-render-loop world-pause-menu-setup :world)
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

(defn load-water-model [dev file]
  (let [
      { :keys [gl storage get-time] } dev
      shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/water_fragment.glsl")
      graphics (graph/load-graphics gl storage shader shader)
      uniform (-> shader .player .uniforms (.uniformVector  "time"))
      uniform-load (proxy [Drawing] [] (draw [] (->> (get-time) (repeat 3) (apply math/vec3f) (.load uniform))))
      model (graph/load-model graphics file)
      model-geom (model :model)
      geom (proxy [TransformedDrawing] []
        (transform [proj light root mats]
          (.draw (.play (.player shader) uniform-load))
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

(def unit-body-offset 1)
(def water-level 189)

(defn load-unit [phys-world horse-preset rider-preset rider-color pos look]
  {
    :pos pos
    :phys-world phys-world
    :look look
    :up [0 1 0]
    :model (horse-preset :model)
    :anims (horse-preset :anims)
    :rider-preset rider-preset
    :rider-materials (dat/load-char-materials rider-preset rider-color)
    :anim :idle
    :update (fn [ch in time] ())
    :next (fn [ch in time delta-time]
      (let [
          { :keys [pos up phys-world swimming?] } ch
          { :keys [movement look] } in
          anim (-> movement mat/length zero? (if :idle :walk))
          pos (->> movement (mapv (partial * (if swimming? 8 18) delta-time)) (mapv + pos))
          ray-origin (mapv + pos [0 10 0])
          { :keys [has-hit normal point] } (phys/ray-check phys-world ray-origin [0 -1 0] 100)
          [rx ry rz] point
          swimming? (and has-hit (< (+ unit-body-offset ry) water-level))
          pos (update pos 1 #(if swimming? (- water-level unit-body-offset) %))
          [px py pz] pos
          pos (cond (and has-hit (not swimming?)) [px ry pz] :else pos)
          up (if has-hit (->> delta-time (* 5) (math/lerpv up normal)) up)
          up (if swimming? [0 1 0] up)
        ]
        (assoc ch :anim anim :look look :pos pos :up up :swimming? swimming?)
      )
    )
    :render (fn [ch time]
      (let [
          { :keys [pos look up rider-preset rider-materials] } ch
          [lx ly lz] look
          [ux uy uz] up
          up-proj (mat/dot up look)
          rot-x (Math/sin up-proj)
          rot-y (math/clock lx lz)
          anims (ch :anims)
          { :keys [anim obj-anim speed] } (-> ch :anim anims)
          [anim obj-anim] (mapv #(graph/animate % (* time speed)) [anim obj-anim])
        ]
        (graph/push-matrix)
        (apply graph/translate pos)
        (graph/rotate 0 rot-y 0)
        (graph/rotate rot-x 0 0)
        (graph/rotate 0 Math/PI 0)
        (-> ch :model (graph/animated-model anim obj-anim))
        (graph/apply-matrix (.transform anim "rider"))
        (-> Math/PI (/ 2) (math/rotation 0 0) graph/apply-matrix)
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

(defn world-render-loop [dev res state]
  (generic/generic-render-loop dev res state)
  (-> res :world-water graph/model)
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
      player (--> player :next (player in time delta-time))
      non-players (mapv #(--> % :next (% (dat/ch-move-in % delta-time [0 0 0]) time delta-time)) non-players)
    ]
    (assoc state
      :campos (math/lerpv (state :campos) campos (* delta-time 5))
      :camrot (math/lerpv (state :camrot) camrot (* delta-time 10))
      :camrot-xy camrot-xy
      :camdist camdist
      :player player
      :non-players non-players
    )
  )
)
