(ns nine-clj.datum
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.phys :as phys]
    [nine-clj.input :as input]
    [clojure.core.matrix :as mat]
  ]
  [:import
    [nine.geometry.collada
      ColladaBasicSkinParser
      ColladaBasicAnimationParser
      ColladaBasicMaterialParser
    ]
  ]
)

(defn load-offset-animated-model [gl storage diffuse-shader skin-shader file & offset-geom-names]
  (graph/load-animated-model
    (graph/load-graphics gl storage diffuse-shader skin-shader
      (geom/geom-offset-parser (partial contains? (apply hash-set offset-geom-names)) [0 0 1])
      (ColladaBasicSkinParser.)
      (ColladaBasicAnimationParser.)
      (ColladaBasicMaterialParser.)
    )
    file
  )
)

(defn load-model [gl storage diffuse-shader skin-shader name offset-geom]
  (load-offset-animated-model gl storage diffuse-shader skin-shader (format "res/datum/%s.dae" name) offset-geom)
)

(defn load-anim [storage model-name anim-name]
  (let [
      af (format "res/datum/anims/%s/%s.anim" model-name anim-name)
      mf (format "res/datum/%s.dae" model-name)
    ]
    (vector
      (graph/load-anim-clj storage (partial = "JOINT") af mf)
      (graph/load-anim-clj storage (partial = "NODE") af mf)
    )
  )
)

(defn preset [model-name offset-geom-name & anims]
  {
    :model-name model-name
    :offset-geom offset-geom-name
    :anims anims
  }
)

(def char-presets
  {
    :archer (preset "archer" "Cube_001-mesh"
      "attack"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :fighter (preset "fighter" "Cube_002-mesh"
      "attack"
      "attack_2"
      "block"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :mage (preset "mage" "Cube_002-mesh"
      "attackspell"
      "spherespell"
      "teleportspell"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :ninja (preset "ninja" "Cube_003-mesh"
      "attack"
      "attack_2"
      "attack_3"
      "idle"
      "walk"
      "death"
      "dead"
    )
  }
)

(defn read-preset [storage key]
  (let
    [
      { :keys [
          model-name
          offset-geom
          anims
        ]
      } (char-presets key)
      loader (fn [gl diffuse-shader skin-shader] (load-model gl storage diffuse-shader skin-shader model-name offset-geom))
      anims (
        (comp
          (partial apply hash-map)
          (partial apply concat)
          (partial map vector anims)
          (partial map deref)
        )
        (mapv
          (comp
            future-call
            partial
          )
          (repeat (partial load-anim storage model-name))
          anims
        )
      )
    ] {
      :loader loader
      :anims anims
    }
  )
)

(defn load-preset [gl diffuse-shader skin-shader preset]
  {
    :model ((preset :loader) gl diffuse-shader skin-shader)
    :anims (preset :anims)
  }
)

(defn load-presets [gl storage diffuse-shader skin-shader]
  (let [
      fs
      (mapv
        (comp
          future-call
          partial
        )
        (repeat (partial read-preset storage))
        (keys char-presets)
      )
    ]
    (mapv
      (comp
        (partial load-preset gl diffuse-shader skin-shader)
        deref
      )
      fs
    )
  )
)

(defn render-preset [preset anim time]
  (let [
      { :keys [model anims] } preset
      [skin-anim obj-anim] (anims anim)
      skin-anim (graph/animate skin-anim time)
      obj-anim (graph/animate obj-anim time)
    ]
    (graph/animated-model model skin-anim obj-anim)
  )
)

(defn char-call [ch sym & args]
  (apply (ch sym) (cons ch args))
)

(defn next-char [ch in] (char-call ch :next in))

(defn update-char [ch in] (char-call ch :update in))

(defn render-char [ch] (char-call ch :render))

(defn new-state [anim timer rtimer next]
  { 
    :anim anim
    :start (timer)
    :timer timer
    :rtimer rtimer
    :next next
  }
)

(declare walk-state)

(defn idle-state [timer rtimer]
  (new-state "idle" timer rtimer (fn [s in]
      (let [
          { :keys [movement] } in
        ]
        (cond
          (zero? (mat/length movement)) s
          :else (walk-state timer rtimer)
        )
      )
    )
  )
)

(defn walk-state [timer rtimer]
  (new-state "walk" timer rtimer (fn [s in]
      (let [
          { :keys [movement] } in
        ]
        (cond
          (zero? (mat/length movement)) (idle-state timer rtimer)
          :else s
        )
      )
    )
  )
)

(defn load-char [world preset pos look timer]
  {
    :body (-> world
      (phys/capsule (mapv + [0 3/4 0] pos) [0 0 0] 0.25 3/2 1)
      (phys/set-rotation-enabled false)
    )
    :pos pos
    :look look
    :state (idle-state timer timer)
    :next (fn [s in]
      (let [
          { :keys [body pos look state] } s
          { :keys [movement] } in
          pos (mapv - (phys/get-position body) [0 3/4 0])
          look (cond
            (zero? (mat/length movement)) look
            :else (math/normalize-checked movement)
          )
          state (char-call state :next in)
        ]
        (assoc s
          :pos pos
          :look look
          :state state
        )
      )
    )
    :update (fn [s in]
      (let [
          { :keys [body pos look] } s
          { :keys [movement] } in
        ]
        (phys/move-char body movement)
        ()
      )
    )
    :render (fn [s]
      (let [
          { :keys [state look pos] } s
          { :keys [anim start rtimer] } state
          [lx ly lz] look
        ]
        (when (nil? rtimer) (println "state:" state))
        (graph/push-matrix)
        (graph/apply-matrix (math/rotation 0 (math/clock lx lz) 0))
        (apply graph/translate pos)
        (render-preset preset anim (- (rtimer) start))
        (graph/pop-matrix)
        ()
      )
    )
  }
)

(defn update-player-state [dev state]
  (let [
      { :keys [campos player] } state
      { :keys [keyboard mouse] } dev
      playerpos (player :pos)
      camsub (mapv - campos playerpos)
      camsub (update camsub 1 (constantly 2))
      camsub (if (zero? (mat/length camsub)) [0 1 -1] (mat/normalise camsub))
      [cx cy cz] (mapv - camsub)
      camrot [0 (math/clock cx cz) 0]
      campos (mapv + playerpos (mapv (partial * 5) camsub))
      [wasd-x wasd-y] (input/wasd keyboard)
      cammat (apply math/rotation camrot)
      cam-fwd (math/get-column-3 cammat 2)
      cam-right (math/get-column-3 cammat 0)
      cam-fwd (mapv (partial * wasd-y) cam-fwd)
      cam-right (mapv (partial * wasd-x) cam-right)
      [mov-x mov-y mov-z] (mapv + cam-fwd cam-right)
      movement (math/normalize [mov-x 0 mov-z])

      in { :movement movement }
      player (do (update-char player in) (next-char player in))

      state (assoc state
        :campos campos
        :camrot camrot
        :movement movement
        :player player
      )
    ]
    state
  )
)