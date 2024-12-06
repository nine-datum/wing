(ns wing.game
  [:require
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.phys :as phys]
    [wing.mac :refer [--> funcall]]
  ]
)

(def camdist 5)
(def cam+ [0 1 0])
(def player-offset [0 1 0])

(declare game-loop)
(declare game-render-loop)

(defn make-player [dev res world pos look]
  (let [
      { :keys [player anims] } res
      rot (math/look-rot look)
      pos (mapv + pos player-offset)
      body (phys/capsule world pos rot 0.2 1.8 1)
    ]
    {
      :model player
      :anims anims
      :anim "idle"
      :body body
    }
  )
)

(defn walk-player [player]
  (-> player :body (phys/set-rotation-enabled false))
  (assoc player :state :walk)
)

(defn walk-player-next [player in]
  (-> player :body (phys/move-char (mapv * (in :mov) (repeat 4))))
  player
)

(def next-player-map {
    :walk walk-player-next
  }
)

(defn next-player [p in time]
  (-> p :state next-player-map (funcall p in))
)

(defn render-player [p time]
  (let [
      { :keys [body anim anims model] } p
      anim (-> anim anims :anim (graph/animate time))
    ]
    (graph/push-matrix)
    (-> body phys/get-matrix math/mat4f graph/apply-matrix)
    (apply graph/translate (mapv - player-offset))
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
  )
)

(defn game-setup [dev res]
  (let [
      world (phys/dynamics-world)
      level (-> res :levels :city)
      { :keys [markers model shapes] } level
      start (markers "Start")
      spawn-pos (-> start (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
      spawn-look (-> start (.transformVector (math/vec3f 0 0 1)) math/floats-from-vec3f)
      player (walk-player (make-player dev res world spawn-pos spawn-look))
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
      player (next-player player in time)
      [ax ay] (mapv #(* % dt 3) (-> dev :keyboard input/arrows))
      state (update state :camrot-xy #(mapv + % [(- ay) ax]))
      camrot-xy (state :camrot-xy)
      camrot (conj camrot-xy 0)
      cammat (apply math/rotation camrot)
      campiv (-> player :body phys/get-position)
      campos (->> camdist
        -
        (math/vec3f 0 0)
        (.transformVector cammat)
        math/floats-from-vec3f
        (mapv + cam+ campiv)
      )
      state (assoc state
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
