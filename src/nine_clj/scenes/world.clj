(ns nine-clj.scenes.world
  [:require
    [nine-clj.scenes.generic :as generic]
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
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
      { :keys [world world-pause-menu-setup] } res
    ]
    (generic/generic-setup dev res world-loop world-render-loop world-pause-menu-setup world)
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
      shader (graph/load-shader gl storage "res/shaders/water_vertex.glsl" "res/shaders/water_fragment.glsl")
      graphics (graph/load-graphics gl storage shader shader)
      uniforms (-> shader .player .uniforms)
      time-uniform (.uniformVector uniforms  "time")
      model (graph/load-model graphics file)
    ]
    (fn [time]
      (.draw
        (.play (.player shader)
          (proxy [Drawing] []
            (draw []
              (->> time (repeat 3) (apply math/vec3f) (.load time-uniform))
            )
          )
        )
      )
      (graph/model model)
    )
  )
)

(defn load-animated-preset [dev diffuse-shader skin-shader geom-file idle-file walk-file]
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

(defn load-static-preset [dev diffuse-shader geom-file]
  (let [
      { :keys [gl storage] } dev
      graphics (graph/load-graphics gl storage diffuse-shader diffuse-shader)
      model (graph/load-model graphics geom-file)
    ]
    {
      :model model
    }
  )
)

(defn load-location-preset [dev diffuse-shader file]
  (assoc (load-static-preset dev diffuse-shader file)
    :shapes (->> file
      (geom/read-geom (dev :storage))
      (map #(map % [:vertex :root]))
      (mapv (partial apply phys/geom-shape))
    )
  )
)

(defn load-presets [dev diffuse-shader skin-shader]
  {
    :horse (load-animated-preset dev diffuse-shader skin-shader
      "res/world/horse/horse_run.dae"
      "res/world/horse/horse_idle.dae"
      "res/world/horse/horse_run.dae"
    )
    :ship (load-static-preset dev diffuse-shader
      "res/world/ship/ship.dae"
    )
    :castle (load-location-preset dev diffuse-shader "res/world/castle/castle.dae")
  }
)

(def unit-body-offset 1)
(def water-level 189)
(def water-effect-level 190.1)

(defn water-peek [x z time]
  (-> x (* 0.25) (+ time) Math/sin inc (+ water-effect-level))
)

(defn water-normal [x z time]
  (let [
      a (water-peek x z time)
      b (water-peek (inc x) z time)
      c (water-peek x (inc z) time)
      a [x a z]
      b [(inc x) b z]
      c [x c (inc z)]
      db (mapv - b a)
      dc (mapv - c a)
      n (mat/cross db dc)
    ]
    (math/normalize n)
  )
)

(declare load-ship)

(defn move-pos [pos movement speed delta-time]
  (->> movement (mapv (partial * speed delta-time)) (mapv + pos))
)

(defn unit-move-in [unit delta-time mov]
  (dat/real-move-in unit (unit :move-torq) delta-time mov)
)

(defn load-horse [phys-world horse-preset rider-preset ship-preset rider-color side pos look]
  {
    :preset rider-preset
    :color rider-color
    :side side
    :move-torq 2
    :pos pos
    :phys-world phys-world
    :look look
    :up [0 1 0]
    :model (horse-preset :model)
    :anims (horse-preset :anims)
    :rider-preset rider-preset
    :rider-materials (dat/load-char-materials rider-preset rider-color)
    :anim :idle
    :next (fn [ch in time delta-time]
      (let [
          { :keys [pos up phys-world side] } ch
          { :keys [movement look] } in
          anim (-> movement mat/length zero? (if :idle :walk))
          pos (move-pos pos movement 18 delta-time)
          ray-origin (mapv + pos [0 10 0])
          { :keys [has-hit normal point] } (phys/ray-check phys-world ray-origin [0 -1 0] 100)
          [rx ry rz] point
          swimming? (and has-hit (< (+ unit-body-offset ry) water-effect-level))
          [px py pz] pos
          pos (cond has-hit [px ry pz] :else pos)
          up (if has-hit (->> delta-time (* 5) (math/lerpv up normal)) up)
        ]
        (cond
          swimming? (load-ship phys-world horse-preset rider-preset ship-preset rider-color side pos look)
          :else (assoc ch :anim anim :look look :pos pos :up up)
        )
      )
    )
    :render (fn [ch time]
      (let [
          { :keys [pos look up rider-preset rider-materials] } ch
          [lx ly lz] look
          [ux uy uz] up
          up-proj (mat/dot up look)
          rot-x (-> up-proj Math/sin -)
          rot-y (math/clock lx lz)
          anims (ch :anims)
          { :keys [anim obj-anim speed] } (-> ch :anim anims)
          [anim obj-anim] (mapv #(graph/animate % (* time speed)) [anim obj-anim])
        ]
        (graph/push-matrix)
        (apply graph/translate pos)
        (graph/rotate 0 (+ Math/PI rot-y) 0)
        (graph/rotate rot-x 0 0)
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

(defn load-ship [phys-world horse-preset rider-preset ship-preset rider-color side pos look]
  {
    :preset rider-preset
    :color rider-color
    :side side
    :move-torq 2/3
    :pos pos
    :look look
    :rider-materials (dat/load-char-materials rider-preset rider-color)
    :next (fn [ch in time delta-time]
      (let [
          { :keys [pos side] } ch
          { :keys [look movement] } in
          pos (move-pos pos movement 20 delta-time)
          [px _ pz] pos
          pos (assoc pos 1 (water-peek px pz time))
          ray-origin (mapv + [0 10 0] pos)
          { :keys [has-hit point] } (phys/ray-check phys-world ray-origin [0 -1 0] 100)
          [rx ry rz] point
          swimming? (or (not has-hit) (-> water-effect-level (- unit-body-offset) (> ry)))
        ]
        (cond
          swimming? (assoc ch :pos pos :look look)
          :else (load-horse phys-world horse-preset rider-preset ship-preset rider-color side pos look)
        )
      )
    )
    :render (fn [ch time]
      (let [
          { :keys [pos look rider-materials] } ch
          [lx ly lz] look
          rot-y (math/clock lx lz)
          pos (mapv + pos [0 0.5 0])
          [px _ pz] pos
          up (water-normal px pz time)
          rot-x (-> up (mat/dot look) Math/sin -)
        ]
        (graph/push-matrix)
        (apply graph/translate pos)
        (graph/rotate 0 rot-y 0)
        (graph/rotate rot-x 0 0)
        (-> ship-preset :model graph/model)
        (graph/translate 0 0.2 -2.7)
        (dat/render-preset rider-preset rider-materials "armature|riding_boat" time)
        (graph/pop-matrix)
      )
    )
  }
)

(defn world-loop [dev res state]
  (let [
      { :keys [world-locations location-setup location-enter-menu-setup] } res
      { :keys [player] } state
      pos (player :pos)
      look (player :look)
      close-locations (->> world-locations (filter #(->> % :pos (mapv - pos) mat/length (> 100))))
      location-close? (-> close-locations empty? not)
    ]
    (cond
      location-close? (let [
          location (first close-locations)
          exit-pos (->> (mapv - pos (location :pos)) math/normalize (mapv * (repeat 10)) (mapv + pos))
          exit-state (update state :player #(assoc % :pos exit-pos :look (mapv - look)))
        ]
        (location-enter-menu-setup dev res
          (location-setup dev res player location exit-state)
          exit-state
          state
        )
      )
      :else (generic/generic-loop dev res state)
    )
  )
)

(defn world-render-loop [dev res state]
  (generic/generic-render-loop dev res state)
  (let [
      { :keys [world-water-effect world-locations] } res
      { :keys [time campos] } state
      [cx cy cz] (map #(->> 500/51 (mod %) (- %)) campos)
    ]
    (doseq [l world-locations]
      (graph/push-matrix)
      (apply graph/translate (l :pos))
      (apply graph/rotate (l :rot))
      (apply graph/scale (l :scale))
      (-> l :model graph/model)
      (graph/pop-matrix)
    )
    (graph/push-matrix)
    (graph/translate cx water-effect-level cz)
    (world-water-effect time)
    (graph/pop-matrix)
  )
)

(defn next-world-state [dev res state]
  (let [
      { :keys [keyboard] } dev
      { :keys [player non-players campos camrot time delta-time] } state
      in (unit-move-in player delta-time (dat/cam-rel-movement keyboard camrot))
      player (--> player :next (player in time delta-time))
      non-players (mapv #(--> % :next (% (unit-move-in % delta-time [0 0 0]) time delta-time)) non-players)

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

      state (assoc state
        :campos (math/lerpv (state :campos) campos (* delta-time 5))
        :camrot (math/lerpv (state :camrot) camrot (* delta-time 10))
        :camrot-xy camrot-xy
        :camdist camdist
        :player player
        :non-players non-players
      )
    ]
    state
  )
)
