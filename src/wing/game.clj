(ns wing.game
  [:require
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.phys :as phys]
    [wing.mac :refer [-->]]
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
      :anim "flight"
      :body body
    }
  )
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

(defn next-player [p in time]
  (let [
    ]
    p
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
      player (make-player dev res world spawn-pos spawn-look)
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

(defn game-loop [dev res state]
  (let [
      { :keys [player world] } state
      time (--> dev :get-time ())
      last-time (state :time)
      dt (- time last-time)
      dt (min dt 1/10)
      world (phys/update-world world dt)
      player (next-player player {} time)
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
