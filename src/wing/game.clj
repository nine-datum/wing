(ns wing.game
  [:require
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.phys :as phys]
    [clojure.core.matrix :as mat]
    [wing.mac :refer [--> funcall impl]]
  ]
  [:import
    [nine.geometry Skeleton AnimatedSkeleton]
  ]
)

(def camdist 5)
(def cam+ [0 1 0])
(def player-offset [0 1 0])

(def player-group 16)
(def player-mask (bit-not 0))

(declare game-loop)
(declare game-render-loop)
(declare walk-player)
(declare fly-player)

(defn player-asset [res world pos rot]
  (let [
      { :keys [player anims] } res
    ]
    {
      :body (doto
        (phys/capsule world (mapv + pos player-offset) rot 0.2 1.6 1)
        (phys/set-group world player-group player-mask)
      )
      :model player
      :anims anims
      :world world
    }
  )
)

(defn walk-player [asset]
  (let [
      body (asset :body)
      look (-> body
        phys/get-matrix
        math/mat4f
        (.transformVector (math/vec3f 0 0 1))
        math/floats-from-vec3f
        (assoc 1 0)
        math/normalize
      )
      rot (math/look-rot look)
    ]
    (doto body
      (phys/set-rotation rot)
      (phys/set-rotation-enabled false)
    )
    {
      :asset asset
      :look look
      :state :walk
      :anim "idle"
    }
  )
)

(defn walk-player-next [player in time delta-time]
  (let [
      {:keys [look asset] } player
      { :keys [body world] } asset
      mov (in :mov)
      mz (-> mov mat/length zero?)
      look (cond mz look :else (math/normalize mov))
      anim (if mz "idle" "walk")
      ray-origin (mapv + player-offset (phys/get-position body))
      { :keys [point has-hit?] } (phys/ray-cast world ray-origin [0 -1 0] 3 player-group)
      falling? (not has-hit?)
    ]
    (phys/move-char body (mapv * mov (repeat 4)))
    (cond
      falling? (fly-player asset look)
      :else (assoc player :look look :anim anim)
    )
  )
)

(defn walk-player-render [player time]
  (let [
      { :keys [asset anim look] } player
      { :keys [anims model body] } asset
      anim (-> anim anims :anim (graph/animate time))
      offset (mapv - player-offset)
    ]
    (graph/push-matrix)
    (->> body phys/get-position (mapv + offset) (apply graph/translate))
    (apply graph/look look)
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
  )
)

(defn fly-player [asset look]
  (let [
      { :keys [body] } asset
    ]
    (doto body
      (phys/set-rotation-enabled true)
      (phys/set-rotation (math/look-rot look))
      (phys/apply-local-force [0 -10 0] [0 0 1])
    )
    {
      :asset asset
      :state :fly
      :turn [0 0]
    }
  )
)

(defn fly-player-next [player in time delta-time]
  (update player :turn #(math/lerpv % (-> in :mov (mapv [0 2])) (* 5 delta-time)))
)

(defn fly-player-render [player time]
  (let [
      { :keys [asset anim turn] } player
      { :keys [anims model body] } asset
      offset (mapv - player-offset)
      anim-mix (impl Skeleton transform [bone]
        (let [
            [tx ty] turn
            pose #(-> % anims :anim (.animate time) (.transform bone))
            res (as-> (pose "flight") p
              (cond
                (< tx 0) (.lerp p (pose "left") (- tx))
                (> tx 0) (.lerp p (pose "right") tx)
                :else p
              )
              (cond
                (< ty 0) (.lerp p (pose "back") (- ty))
                (> ty 0) (.lerp p (pose "drop") ty)
                :else p
              )
            )
          ]
          res
        )
      )
    ]
    (graph/push-matrix)
    (-> body phys/get-matrix math/mat4f graph/apply-matrix)
    (apply graph/translate offset)
    (graph/animated-model model anim-mix nil)
    (graph/pop-matrix)
  )
)

(def player-map {
    :walk { :next walk-player-next :render walk-player-render }
    :fly { :next fly-player-next :render fly-player-render }
  }
)

(defn next-player [p in time delta-time]
  (-> p :state player-map :next (funcall p in time delta-time))
)

(defn render-player [p time]
  (-> p :state player-map :render (funcall p time))
)

(defn game-setup [dev res]
  (let [
      world (phys/dynamics-world)
      level (-> res :levels :city)
      { :keys [markers model shapes] } level
      start (markers "Start")
      spawn-pos (-> start (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
      spawn-look (-> start (.transformVector (math/vec3f 0 0 1)) math/floats-from-vec3f)
      spawn-rot (math/look-rot spawn-look)
      player (walk-player (player-asset res world spawn-pos spawn-rot))
    ]
    (doseq [s shapes] (phys/add-rigid-body world (s :shape) [0 0 0] [0 0 0] 0))
    {
      :loop game-loop
      :player player
      :time (--> dev :get-time ())
      :camrot-xy [0 0]
      :model model
      :world world
    }
  )
)

(defn move-in [mov]
  { :mov mov }
)

(defn game-loop [dev res state]
  (let [
      { :keys [player world] } state
      { :keys [keyboard get-time] } dev 
      time (get-time)
      last-time (state :time)
      dt (- time last-time)
      dt (min dt 1/10)
      world (phys/update-world world dt)
      camrot (get state :camrot [0 0 0])
      cammat-y (math/rotation 0 (camrot 1) 0)
      in (->> keyboard input/wasd
        (apply math/x0y)
        (apply math/vec3f)
        (.transformVector cammat-y)
        math/floats-from-vec3f
        move-in
      )
      player (next-player player in time dt)
      [ax ay] (mapv #(* % dt 3) (-> dev :keyboard input/arrows))
      state (update state :camrot-xy #(mapv + % [(- ay) ax]))
      camrot-xy (state :camrot-xy)
      camrot (conj camrot-xy 0)
      cammat (apply math/rotation camrot)
      campiv (-> player :asset :body phys/get-position)
      campos (->> camdist
        -
        (math/vec3f 0 0)
        (.transformVector cammat)
        math/floats-from-vec3f
        (mapv + cam+ campiv)
      )
      state (assoc state
        :player player
        :time time
        :campos campos
        :camrot camrot
        :world world
      )
    ]
    (game-render-loop dev res state)
    state
  )
)

(defn game-render-loop [dev res state]
  (let [
      { :keys [gl width height] } dev
      w (width)
      h (height)
      { :keys [campos camrot] } state
    ]
    (.clearDepth gl)
    (.clearColor gl 1/2 1/2 1 0)
    (graph/projection (math/perspective w h (* Math/PI 1/2) 0.3 5000))
    (graph/camera (math/first-person-camera campos camrot))
    (-> [-1 -1 -0.5] math/normalize graph/world-light)
    (-> state :model graph/model)
    (-> state :player (render-player (state :time)))
  )
)
