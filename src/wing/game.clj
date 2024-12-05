(ns wing.game
  [:require
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.mac :refer [-->]]
  ]
)

(def camdist 5/2)
(def cam+ [0 2 0])

(declare game-loop)
(declare game-render-loop)

(defn make-player [dev res pos look]
  (let [
      { :keys [player anims] } res
    ]
    {
      :model player
      :anims anims
      :anim "flight"
      :pos pos
      :look look
    }
  )
)

(defn render-player [p time]
  (let [
      { :keys [pos look anim anims model] } p
      anim (-> anim anims :anim (graph/animate time))
    ]
    (graph/push-matrix)
    (apply graph/translate pos)
    (apply graph/look look)
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
  )
)

(defn game-setup [dev res]
  (let [
      player (make-player dev res [0 0 0] [0 0 1])
    ]
    {
      :loop game-loop
      :player player
      :time (--> dev :get-time ())
      :camrot-xy [0 0]
    }
  )
)

(defn game-loop [dev res state]
  (let [
      time (--> dev :get-time ())
      last-time (state :time)
      dt (- time last-time)
      dt (min dt 1/10)
      time (+ last-time dt)
      [ax ay] (mapv #(* % dt 3) (-> dev :keyboard input/arrows))
      state (update state :camrot-xy #(mapv + % [(- ay) ax]))
      camrot-xy (state :camrot-xy)
      camrot (conj camrot-xy 0)
      cammat (apply math/rotation camrot)
      campos (->> camdist
        -
        (math/vec3f 0 0)
        (.transformVector cammat)
        math/floats-from-vec3f
        (mapv + cam+)
      )
      state (assoc state
        :time time
        :campos campos
        :camrot camrot
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
    (graph/projection (math/perspective w h (* Math/PI 2/3) 0.3 1000))
    (graph/camera (math/first-person-camera campos camrot))
    (graph/world-light [0 -1 0])
    (-> state :player (render-player (state :time)))
  )
)
