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

(defn mix-player-anims [anims turn time]
  (impl Skeleton transform [bone]
    (let [
        [tx ty] turn
        pose #(-> % anims :anim (.animate time) (.transform bone))
        res (as-> (pose "flight") p
          (cond
            (< ty 0) (.lerp p (pose "back") (- ty))
            (> ty 0) (.lerp p (pose "drop") ty)
            :else p
          )
          (cond
            (< tx 0) (.lerp p (pose "left") (- tx))
            (> tx 0) (.lerp p (pose "right") tx)
            :else p
          )
        )
      ]
      res
    )
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
  (let [
      { :keys [turn asset] } player
      { :keys [anims body] } asset
      turn (math/lerpv turn (-> in :raw-mov math/normalize) (* 2 delta-time))
      anim-mix (mix-player-anims anims turn time)
      fwd-wing-rot (fn [sig [tx ty]] [(* tx sig Math/PI 1/8) 0 (-> tx - (* Math/PI 1/4))])
      back-wing-rot (fn [[tx ty]] [(-> ty - inc (* Math/PI 1/6)) 0 0])
      wing-area (fn [[tx ty] area] (-> ty - inc (/ 2) (* area)))
      wings [
        { ; left wing
          :area 1/2
          :rel [-1 0 0]
          :norm [0 0 -1]
          :rot-fn (partial fwd-wing-rot -1)
          :area-fn wing-area
        }
        { ; right wing
          :area 1/2
          :rel [1 0 0]
          :norm [0 0 -1]
          :rot-fn (partial fwd-wing-rot 1)
          :area-fn wing-area
        }
        {
          ; back wing
          :area 1
          :rel [0 -2 0]
          :norm [0 0 -1]
          :rot-fn back-wing-rot
          :area-fn wing-area
        }
        { ; tail wing
          :area 1
          :rel [0 -1 0]
          :norm [0 0 -1]
          :rot-fn (constantly [0 0 0])
          :area-fn (fn [t a] a)
        }
        { ; tail wing 1
          :area 1
          :rel [0 -1 0]
          :norm [1 0 0]
          :rot-fn (constantly [0 0 0])
          :area-fn (fn [t a] a)
        }
      ]
      body-mat (-> body phys/get-matrix math/mat4f)
      wings (->> wings
        (map #(let [
              { :keys [area rel norm rot-fn area-fn] } %
              mat (->> turn rot-fn (apply math/rotation) (.mul body-mat))
              area (area-fn turn area)
              pos (math/floats-from-vec3f
                (.transformPoint mat (apply math/vec3f rel))
              )
              norm (->> (apply math/vec3f norm)
                (.transformVector mat)
                math/floats-from-vec3f
                math/normalize
              )
              vel (phys/get-point-velocity body pos)
              norm (->> norm (mat/dot vel) Math/signum - repeat (mapv * norm))
              ap (-> vel math/normalize (mat/dot norm) -)
              vm2 (->> vel mat/length); (repeat 2) (apply *))
              force (mapv (partial * ap vm2 area) norm)
            ]
            (phys/apply-world-force body force pos)
          )
        )
        dorun
      )
    ]
    (assoc player :turn turn)
  )
)

(defn fly-player-render [player time]
  (let [
      { :keys [asset turn] } player
      { :keys [anims model body] } asset
      offset (mapv - player-offset)
      anim (mix-player-anims anims turn time)
    ]
    (graph/push-matrix)
    (-> body phys/get-matrix math/mat4f graph/apply-matrix)
    (apply graph/translate offset)
    (graph/animated-model model anim nil)
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

(defn move-in [raw-mov mov]
  { :mov mov :raw-mov raw-mov }
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
      wasd (input/wasd keyboard)
      in (->> wasd
        (apply math/x0y)
        (apply math/vec3f)
        (.transformVector cammat-y)
        math/floats-from-vec3f
        (move-in wasd)
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
    (cond
      (input/escape-up keyboard) (-> res :menu-setup (funcall dev res))
      :else state
    )
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
