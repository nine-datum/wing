(ns wing.game
  [:require
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.phys :as phys]
    [wing.client :as client]
    [wing.server :as server]
    [clojure.core.matrix :as mat]
    [wing.mac :refer [--> funcall impl]]
  ]
  [:import
    [nine.geometry Skeleton AnimatedSkeleton]
  ]
)

(def camdist 2)
(def max-camdist 3)
(def cam+ [0 1 0])
(def player-offset [0 1 0])

(def player-group 16)
(def player-mask (bit-not 0))

(declare game-loop)
(declare game-render-loop)
(declare walk-player)
(declare fly-player)

(defn left-player-controls [keyboard sym]
  (case sym
    :jump (input/space-down keyboard)
    :mov (input/wasd keyboard)
  )
)

(defn right-player-controls [keyboard sym]
  (case sym
    :jump (input/right-shift-down keyboard)
    :mov (input/arrows)
  )
)

(def player-controls {
    :left left-player-controls
    :right right-player-controls
  }
)

(defn player-asset [world pos rot controls color]
  {
    :body (doto
      (phys/capsule world (mapv + pos player-offset) rot 0.2 1.6 1)
      (phys/set-group world player-group player-mask)
    )
    :world world
    :controls controls
    :color color
    :camrot [0 0 0]
    :campos (mapv + pos cam+ [0 0 (- camdist)])
  }
)

(defn player-model [dev res player]
  (graph/replace-materials
    (dev :gl)
    (res :player)
    { "ColorA-material" (-> player :color) }
  )
)

(defn next-camera [player time delta-time]
  (let [
      { :keys [asset] } player
      { :keys [campos camrot] } asset
      pos (->> asset :body phys/get-position (mapv + cam+))
      delt (mapv - pos campos)
      camlook (math/normalize delt)
      new-camrot (math/look-rot camlook)
      new-campos #(->> camlook (map -) (map * (repeat %)) (mapv + pos))
      new-campos (comp #(update % 1 (partial max (pos 1))) new-campos)
      campos (cond
        (> max-camdist (mat/length delt)) (math/lerpv campos (new-campos camdist) (* 3 delta-time))
        :else (new-campos max-camdist)
      )
      camrot (math/lerpv-angle camrot new-camrot (* 3 delta-time))
    ]
    (assoc player :asset (assoc asset :campos campos :camrot camrot))
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
      :color (asset :color)
      :pos (phys/get-position body)
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
      pos (phys/get-position body)
    ]
    (phys/move-char body (mapv * mov (repeat 4)))
    (cond
      falling? (fly-player asset look)
      :else (assoc player :look look :anim anim :pos pos)
    )
  )
)

(defn walk-player-render [player dev res time]
  (let [
      { :keys [anims] } res
      { :keys [anim look pos] } player
      anim (-> anim anims :anim (graph/animate time))
      offset (mapv - player-offset)
      model (player-model dev res player)
    ]
    (graph/push-matrix)
    (->> pos (mapv + offset) (apply graph/translate))
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
      { :keys [body color] } asset
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
      :color color
      :mat (phys/get-matrix body)
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
      back-wing-rot (fn [[tx ty]] [(-> ty - (max 0) (* Math/PI 1/6)) 0 0])
      wing-area (fn [[tx ty] area] (-> ty - inc (/ 2) (* area)))
      wings [
        { ; left wing
          :area 2
          :rel [-1 0 0]
          :norm [0 0 -1]
          :rot-fn (partial fwd-wing-rot -1)
          :area-fn wing-area
        }
        { ; right wing
          :area 2
          :rel [1 0 0]
          :norm [0 0 -1]
          :rot-fn (partial fwd-wing-rot 1)
          :area-fn wing-area
        }
        {
          ; back wing
          :area 4
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
      mat-vec (phys/get-matrix body)
      body-mat (math/mat4f mat-vec)
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
    (assoc player :turn turn :mat mat-vec)
  )
)

(defn fly-player-render [player dev res time]
  (let [
      { :keys [anims] } res
      { :keys [turn mat] } player
      { :keys [gl] } dev
      offset (mapv - player-offset)
      anim (mix-player-anims anims turn time)
      model (player-model dev res player)
    ]
    (graph/push-matrix)
    (-> mat math/mat4f graph/apply-matrix)
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
  (-> p :state player-map :next (funcall p in time delta-time) (next-camera time delta-time))
)

(defn render-player [p dev res time]
  (-> p :state player-map :render (funcall p dev res time))
)

(defn game-setup [dev res player-num]
  (let [
      world (phys/dynamics-world)
      level (-> res :levels :city)
      { :keys [markers model shapes] } level
      start (markers "Start")
      spawn-pos (-> start (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
      spawn-look (-> start (.transformVector (math/vec3f 0 0 1)) math/floats-from-vec3f)
      spawn-rot (math/look-rot spawn-look)
      spawn-pos-a (mapv + [2 0 0] spawn-pos)
      players (take player-num (vector
        (walk-player (player-asset world spawn-pos spawn-rot :left [1 0 0 1]))
        (walk-player (player-asset world spawn-pos-a spawn-rot :right [0 0 1 1]))
      ))
    ]
    (doseq [s shapes] (phys/add-rigid-body world (s :shape) [0 0 0] [0 0 0] 0))
    {
      :loop game-loop
      :exit (constantly nil)
      :players players
      :player-num player-num
      :time (--> dev :get-time ())
      :model model
      :world world
    }
  )
)

(defn make-netplayer [player]
  (dissoc player :asset)
)

(defn client-loop [dev res state]
  (cond (client/running?)
    (let [
        net-players (->> (client/got) vals (apply concat))
        state (game-loop dev res state)
      ]
      (client/send! (mapv make-netplayer (state :players)))
      (assoc state :net-players net-players)
    )
    :else ((res :error-menu-setup) dev res "Ошибка подключения к серверу")
  )
)

(defn server-loop [dev res state]
  (cond (server/running?)
    (client-loop dev res state)
    :else ((res :error-menu-setup) dev res "Ошибка работы сервера")
  )
)

(defn client-setup [dev res addr name]
  (client/start-client addr (res :net-port) name)
  (assoc (game-setup dev res 1)
    :loop client-loop
    :exit client/close-client
  )
)

(defn server-setup [dev res name]
  (server/start-server (res :net-port) (res :udp-port) name)
  (assoc (client-setup dev res "localhost" name)
    :loop server-loop
    :exit (fn [] (client/close-client) (server/close-server))
  )
)

(defn move-in [raw-mov mov]
  { :mov mov :raw-mov raw-mov }
)

(defn player-in [player keyboard]
  (let [
      camrot (-> player :asset :camrot)
      cammat-y (math/rotation 0 (camrot 1) 0)
      controls (-> player :asset :controls player-controls)
      mov (controls keyboard :mov)
      in (->> mov
        (apply math/x0y)
        (apply math/vec3f)
        (.transformVector cammat-y)
        math/floats-from-vec3f
        (move-in mov)
      )
      jump (controls keyboard :jump)
      in (cond jump (assoc in :action :jump) :else in)
    ]
    in
  )
)

(defn game-loop [dev res state]
  (let [
      { :keys [players world] } state
      { :keys [keyboard get-time] } dev 
      time (get-time)
      last-time (state :time)
      dt (- time last-time)
      dt (min dt 1/10)
      world (phys/update-world world dt)
      players (mapv
        #(next-player % (player-in % keyboard) time dt)
        players
      )
      [ax ay] [0 0];(mapv #(* % dt 3) (-> dev :keyboard input/arrows))
      state (update state :camrot-xy #(mapv + % [(- ay) ax]))
      state (assoc state
        :players players
        :time time
        :world world
      )
    ]
    (game-render-loop dev res state)
    (cond
      (input/escape-up keyboard) (do
        (-> state :exit funcall)
        (-> res :menu-setup (funcall dev res))
      )
      :else state
    )
  )
)

(defn game-render-loop [dev res state]
  (let [
      { :keys [gl width height] } dev
      w (width)
      h (height)
      { :keys [players net-players player-num] } state
      hw (/ w 2)
      viewports {
        1 [[0 0 w h]]
        2 [
          [0 0 hw h]
          [hw 0 hw h]
        ]
      }
    ]
    (.clearDepth gl)
    (.clearColor gl 1/2 1/2 1 0)
    (mapv (fn [p v]
      (apply graph/viewport (cons gl v))
      (graph/projection (math/perspective (nth v 2) h (* Math/PI 1/2) 0.3 5000))
      (graph/camera (math/first-person-camera (-> p :asset :campos) (-> p :asset :camrot)))
      (-> [-1 -1 -0.5] math/normalize graph/world-light)
      (-> state :model graph/model)
      (doseq [p (concat players net-players)] (render-player p dev res (state :time)))
    ) players (-> player-num viewports cycle))
    (graph/viewport gl 0 0 w h)
  )
)
