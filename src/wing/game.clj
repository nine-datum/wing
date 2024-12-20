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
(def max-camdist 4)
(def run-camdist 4)
(def max-run-camdist 4)
(def cam+ [0 1 0])
(def player-offset [0 1 0])
(def flip-length 1/2)

(def player-group 16)
(def player-mask (bit-not 0))

(def flight-enter-speed 10)
(def flight-leave-speed 5)

(declare game-loop)
(declare game-render-loop)
(declare walk-player)
(declare run-player)
(declare jump-player)
(declare fall-player)
(declare lost-player)
(declare fly-player)
(declare parachute-player)
(declare flip-player)

(defn left-player-controls [keyboard sym]
  (case sym
    :jump (input/space-up keyboard)
    :mov (input/wasd keyboard)
    :run (input/shift-down keyboard)
    :spec (input/tab-up keyboard)
  )
)

(defn right-player-controls [keyboard sym]
  (case sym
    :jump (input/backspace-up keyboard)
    :mov (input/arrows keyboard)
    :run (input/enter-down keyboard)
    :spec (input/shift-down keyboard)
  )
)

(def player-controls {
    :left left-player-controls
    :right right-player-controls
  }
)

(defn reset-body [body]
  (phys/set-gravity body [0 -9.8 0])
)

(defn reset-asset-body [asset]
  (-> asset :body reset-body)
  asset
)

(defn player-asset [world pos rot controls color]
  {
    :body (doto
      (phys/capsule world (mapv + pos player-offset) rot 0.2 1.6 1)
      (phys/set-group world player-group player-mask)
      reset-body
    )
    :world world
    :controls controls
    :color color
    :camrot [0 0 0]
    :campos (mapv + pos cam+ [0 0 (- camdist)])
  }
)

(defn make-player-model [gl model color]
  (graph/replace-materials
    gl
    model
    { "ColorA-material" color }
  )
)

(defn next-camera [player time delta-time camdist max-camdist]
  (let [
      { :keys [asset] } player
      { :keys [campos camrot] } asset
      pos (->> asset :body phys/get-position (mapv + cam+))
      delt (mapv - pos campos)
      camlook (math/normalize delt)
      new-camrot (math/look-rot camlook)
      new-campos #(as-> camlook c
        (mapv - c)
        (update c 1 (partial max 0))
        (math/normalize c)
        (map * c (repeat %))
        (mapv + c pos)
      )
      campos (cond
        (-> delt mat/length (> max-camdist)) (new-campos max-camdist)
        :else (math/lerpv campos (new-campos camdist) (* 3 delta-time))
      )
      camrot (math/lerpv-angle camrot new-camrot (* 3 delta-time))
    ]
    (assoc player :asset (assoc asset :campos campos :camrot camrot))
  )
)

(defn next-walk-camera [player time delta-time]
  (next-camera player time delta-time camdist max-camdist)
)

(defn next-run-camera [player time delta-time]
  (next-camera player time delta-time run-camdist max-run-camdist)
)

(defn next-parachute-camera [player time delta-time]
  (let [
      { :keys [asset] } player
      { :keys [campos camrot body] } asset
      campiv (mapv + cam+ (phys/get-position body))
      vel (phys/get-velocity body)
      camdir (->
        body phys/get-matrix math/mat4f
        (.transformVector (math/vec3f 0 0 1))
        math/floats-from-vec3f
        math/normalize
      )
      camdir (->> vel (map * (repeat 1/10)) (mapv + camdir) math/normalize)
      new-campos (mapv - campiv (map * camdir (repeat max-camdist)))
      new-camrot (math/look-rot camdir)
      campos (math/lerpv campos new-campos (* 3 delta-time))
      camrot (math/lerpv-angle camrot new-camrot (* 3 delta-time))
    ]
    (assoc player :asset (assoc asset :campos campos :camrot camrot))
  )
)

(defn proc-wing [body body-mat wing]
  (let [
      { :keys [area rel norm rot onesided] } wing
      mat (.mul body-mat (apply math/rotation rot))
      pos (math/floats-from-vec3f
        (.transformPoint mat (apply math/vec3f rel))
      )
      norm (->> (apply math/vec3f norm)
        (.transformVector mat)
        math/floats-from-vec3f
        math/normalize
      )
      vel (phys/get-point-velocity body pos)
      dot (mat/dot vel norm)
      skip? (and onesided (< dot 0))
      norm (->> dot Math/signum - repeat (mapv * norm))
      ap (-> vel math/normalize (mat/dot norm) -)
      vm2 (mat/length vel)
      force (mapv (partial * ap vm2 area) norm)
    ]
    (when-not skip? (phys/apply-world-force body force pos))
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

(defn body-look [body]
  (-> body
    phys/get-matrix
    math/mat4f
    (.transformVector (math/vec3f 0 0 1))
    math/floats-from-vec3f
    (assoc 1 0)
    math/normalize
  )
)

(defn on-ground? [player]
  (let [
      { :keys [world body] } (player :asset)
      pos (phys/get-position body)
    ]
    (->
      (phys/sphere-check world pos (mapv - pos [0 1 0]) 1/4 player-group)
      empty? not
    )
  )
)

(defn check-wall [player]
  (let [
      { :keys [body world] } (player :asset)
      pos (phys/get-position body)
      down (-> body
        phys/get-matrix
        math/mat4f
        (.transformVector (math/vec3f 0 -1 0))
        math/floats-from-vec3f
        math/normalize
      )
      up (mapv - down)
      d (mapv * down (repeat 3/2))
      res (phys/sphere-cast world pos (mapv + pos d) 1/4 player-group)
      { :keys [has-hit? normal] } res
      dot (mat/dot normal up)
    ]
    (when (and has-hit? (> dot 1/3) (-> normal (nth 1) Math/abs (< 1/4))) res)
  )
)

(defn move-player [asset state move-anim move-speed]
  (let [
      body (asset :body)
      look (body-look body)
      rot (math/look-rot look)
    ]
    (doto body
      (phys/set-rotation rot)
      (phys/set-angular-velocity [0 0 0])
      (phys/set-velocity [0 0 0])
      (phys/set-rotation-enabled false)
    )
    {
      :asset asset
      :look look
      :state state
      :anim "idle"
      :move-anim move-anim
      :move-speed move-speed
      :color (asset :color)
      :pos (phys/get-position body)
    }
  )
)

(defn move-player-next [player in time delta-time]
  (let [
      {:keys [look asset move-anim move-speed] } player
      { :keys [body world] } asset
      mov (in :mov)
      mz (-> mov mat/length zero?)
      look (cond mz look :else (math/normalize mov))
      anim (if mz "idle" move-anim)
      falling? (-> player on-ground? not)
      pos (phys/get-position body)
    ]
    (phys/move-char body (mapv * mov (repeat move-speed)))
    (phys/set-rotation body (math/look-rot look))
    (cond
      falling? (fall-player asset)
      :else (assoc player :look look :anim anim :pos pos)
    )
  )
)

(defn pos-look-player-render [player dev res time]
  (let [
      { :keys [anims player-model] } res
      { :keys [anim pos look color] } player
      anim (-> anim anims :anim (graph/animate time))
      offset (mapv - player-offset)
      model (make-player-model (dev :gl) player-model color)
    ]
    (graph/push-matrix)
    (->> pos (mapv + offset) (apply graph/translate))
    (apply graph/look look)
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
  )
)

(defn anim-player-render [player dev res time anim]
  (let [
      { :keys [model mat color] } player
      model (if model model :player-model)
      offset (mapv - player-offset)
      model (make-player-model (dev :gl) (res model) color)
    ]
    (graph/push-matrix)
    (-> mat math/mat4f graph/apply-matrix)
    (apply graph/translate offset)
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
  )
)

(defn mat-player-render [player dev res time]
  (anim-player-render player dev res time
    (-> player :anim ((res :anims)) :anim (graph/animate time))
  )
)

(defn mat-player-lerp [a b t]
  (assoc b :mat (math/lerpv (a :mat) (b :mat) t))
)

(defn move-player-render [player dev res time]
  (pos-look-player-render player dev res time)
)

(defn move-player-lerp [a b t]
  (let [
      pos (math/lerpv (a :pos) (b :pos) t)
      look (math/normalize (math/lerpv (a :look) (b :look) t))
    ]
    (assoc b :pos pos :look look)
  )
)

(defn walk-player [asset]
  (move-player asset :walk "walk" 4)
)

(defn walk-player-next [player in time delta-time]
  (let [
      next (move-player-next player in time delta-time)
    ]
    (case (in :action)
      :jump (-> player :asset jump-player)
      :run (-> player :asset run-player)
      next
    )
  )
)

(defn run-player [asset]
  (move-player asset :run "run" 10)
)

(defn run-player-next [player in time delta-time]
  (let [
      next (move-player-next player in time delta-time)
    ]
    (case (in :action)
      :jump (-> player :asset jump-player)
      :run next
      (-> player :asset walk-player)
    )
  )
)

(defn jump-player [asset]
  (let [
      { :keys [body color] } asset
      up (-> body
        phys/get-matrix
        math/mat4f
        (.transformVector (math/vec3f 0 1 0))
        math/floats-from-vec3f
        math/normalize
      )
      force (mapv * up (repeat 10))
    ]
    (phys/set-rotation-enabled body false)
    (phys/update-velocity body #(mapv + % force))
    {
      :asset asset
      :color color
      :age 0
      :state :jump
      :anim "jump"
      :mat (phys/get-matrix body)
    }
  )
)

(defn jump-player-next [player in time delta-time]
  (let [
      { :keys [asset age] } player
      { :keys [body world] } asset
      falling? (> age 2/3)
      age (+ age delta-time)
    ]
    (cond
      falling? (fall-player asset)
      :else (assoc player :age age :mat (phys/get-matrix body))
    )
  )
)

(defn jump-player-render [player dev res time]
  (let [
      { :keys [anim age] } player
    ]
    (mat-player-render player dev res age)
  )
)

(defn jump-player-lerp [a b t]
  (assoc (mat-player-lerp a b t) :age (math/lerp (a :age) (b :age) t))
)

(defn wallrun-player [asset wall]
  (let [
      body (asset :body)
      norm (wall :normal)
      point (wall :point)
      plane-mat (->> norm math/look-rot (apply math/rotation))
      plane-right (-> plane-mat (.transformVector (math/vec3f -1 0 0)) math/floats-from-vec3f)
      plane-up (-> plane-mat (.transformVector (math/vec3f 0 1 0)) math/floats-from-vec3f)
      dir (-> asset :body phys/get-look)
      px (mat/dot dir plane-right)
      py (mat/dot dir plane-up)
      [px py] (math/normalize [px py])
      rot-mat (math/mul
        plane-mat
        (math/rotation (/ Math/PI 2) 0 0)
        (math/rotation 0 (+ (math/clock px py) Math/PI) 0)
      )
      pos (phys/get-position body)
      mat (->> rot-mat
        (.mul (apply math/translation (map + point norm)))
        math/floats-from-mat4f
      )
    ]
    (phys/set-matrix body mat)
    (phys/set-rotation-enabled body false)
    (phys/set-gravity body [0 0 0])
    (phys/set-velocity body [0 0 0])
    (phys/set-angular-velocity body [0 0 0])
    {
      :asset asset
      :state :wallrun
      :color (asset :color)
      :mat mat
      :age 0
      :pr plane-right
      :pu plane-up
      :pdir [px py]
      :anim "run"
    }
  )
)

(defn wallrun-player-next [player in time delta-time]
  (let [
      body (-> player :asset :body)
      w (check-wall player)
      mat (phys/get-matrix body)
      { :keys [pr pu pdir] } player
      [px py] pdir
      vel (->>
        (map * pr (repeat px))
        (map + (map * pu (repeat py)))
        (mapv * (repeat 10))
      )
      age (-> player :age (+ delta-time))
    ]
    (phys/set-velocity body vel)
    (cond
      (or (not w) (= (in :action) :jump) (> age 3)) (-> player :asset reset-asset-body flip-player)
      (on-ground? player) (-> player :asset reset-asset-body walk-player)
      :else (assoc player :mat mat :age age)
    )
  )
)

(defn flip-player [asset]
  (assoc (jump-player asset) :state :flip)
)

(defn flip-player-next [player in time delta-time]
  (let [
      body (-> player :asset :body)
      mat (phys/get-matrix body)
      age (-> player :age (+ delta-time))
    ]
    (phys/apply-local-force body [Math/PI 0 0] [0 (/ flip-length) 0])
    (cond
      (> age flip-length) (-> player :asset fall-player)
      :else (assoc player :mat mat :age age)
    )
  )
)

(defn flip-player-render [player dev res time]
  (let [
      anims (res :anims)
      age (player :age)
      jump (-> "jump" anims :anim (graph/animate age))
      ball (-> "ball" anims :anim (graph/animate time))
      fall (-> "fall" anims :anim (graph/animate time))
      n (- (/ age flip-length) 2/3)
      n (if (< n 0) (/ n 2/3) (* n 3))
      anim (impl Skeleton transform [bone]
        (cond
          (< n 0) (.lerp (.transform jump bone) (.transform ball bone) (inc n))
          :else (.lerp (.transform ball bone) (.transform fall bone) n)
        )
      )
    ]
    (anim-player-render player dev res time anim)
  )
)

(defn fall-player [asset]
  {
    :asset asset
    :state :fall
    :anim "fall"
    :color (asset :color)
    :mat (-> asset :body phys/get-matrix)
  }
)

(defn fall-player-next [player in time delta-time]
  (let [
      w (check-wall player)
      mov (->> in :raw-mov (apply math/x0y))
    ]
    (doto (-> player :asset :body)
      (phys/apply-local-force mov [0 5 0])
    )
    (cond
      (on-ground? player) (-> player :asset walk-player)
      (= (in :action) :jump) (-> player :asset parachute-player)
      (= (in :action) :spec) (-> player :asset lost-player)
      w (wallrun-player (player :asset) w)
      :else (assoc player :mat (-> player :asset :body phys/get-matrix))
    )
  )
)

(defn lost-player [asset]
  {
    :asset asset
    :state :lost
    :color (asset :color)
    :mat (-> asset :body phys/get-matrix)
    :anim "lost"
    :model :player-wings-model
    :age 0
  }
)

(defn lost-player-next [player in time delta-time]
  (let [
      body (-> player :asset :body)
      mat (phys/get-matrix body)
      mat4 (math/mat4f mat)
      pos (-> mat4 (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
      src (-> mat4 (.transformVector (math/vec3f 0 1 0))
        math/floats-from-vec3f math/normalize
      )
      vel (phys/get-velocity body)
      dst (math/normalize vel)
      dot (mat/dot src dst)
      delta (mapv - dst src)
      grounded? (on-ground? player)
    ]
    (phys/apply-world-force body delta (mapv + pos src))
    (cond
      grounded? (-> player :asset walk-player)
      (and (> (mat/length vel) flight-enter-speed) (> dot 0.9)) (do
        (phys/set-angular-velocity body [0 0 0])
        (-> player :asset fly-player)
      )
      (= (in :action) :jump) (-> player :asset parachute-player)
      :else (assoc player :mat mat)
    )
  )
)

(defn fly-player [asset]
  (let [
      { :keys [body color] } asset
    ]
    (doto body
      (phys/set-rotation-enabled true)
      (phys/apply-local-force [0 -10 0] [0 0 1])
    )
    {
      :asset asset
      :state :fly
      :turn [0 -1]
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
      [tx ty] turn
      fwd-wing-rot (fn [sig] [(* tx sig Math/PI 1/8) 0 (-> tx - (* Math/PI 1/4))])
      back-wing-rot [(-> ty - (max 0) (* Math/PI 1/6)) 0 0]
      back-wing-pos-z (-> ty - (max 0))
      wing-area (fn [area] (-> ty - inc (/ 2) (* area)))
      wings [
        { ; left wing
          :area (wing-area 2)
          :rel [-1 0 0]
          :norm [0 0 -1]
          :rot (fwd-wing-rot -1)
        }
        { ; right wing
          :area (wing-area 2)
          :rel [1 0 0]
          :norm [0 0 -1]
          :rot (fwd-wing-rot 1)
        }
        {
          ; back wing
          :area (wing-area 4)
          :rel [0 -2 back-wing-pos-z]
          :norm [0 0 -1]
          :rot back-wing-rot
        }
        { ; tail wing
          :area 1
          :rel [0 -1 0]
          :norm [0 0 -1]
          :rot [0 0 0]
        }
        { ; tail wing 1
          :area 1
          :rel [0 -1 0]
          :norm [1 0 0]
          :rot [0 0 0]
        }
      ]
      mat-vec (phys/get-matrix body)
      body-mat (math/mat4f mat-vec)
    ]
    (doseq [w wings] (proc-wing body body-mat w))
    (cond
      (-> body phys/get-velocity mat/length (< flight-leave-speed)) (lost-player asset)
      :else
      (case (in :action)
        :jump (parachute-player asset)
        :spec (fall-player asset)
        (assoc player :turn turn :mat mat-vec)
      )
    )
  )
)

(defn fly-player-render [player dev res time]
  (let [
      { :keys [anims player-wings-model] } res
      { :keys [turn mat color] } player
      { :keys [gl] } dev
      offset (mapv - player-offset)
      anim (mix-player-anims anims turn time)
      model (make-player-model gl player-wings-model color)
    ]
    (graph/push-matrix)
    (-> mat math/mat4f graph/apply-matrix)
    (apply graph/translate offset)
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
  )
)

(defn fly-player-lerp [a b t]
  (let [
      turn (math/lerpv (a :turn) (b :turn) t)
      mat (math/lerpv (a :mat) (b :mat) t)
    ]
    (assoc b :turn turn :mat mat)
  )
)

(defn parachute-player [asset]
  (let [
      { :keys [color body] } asset
    ]
    {
      :asset asset
      :state :parachute
      :color color
      :anim "parachuting"
      :mat (phys/get-matrix body)
    }
  )
)

(defn parachute-player-next [player in time delta-time]
  (let [
      { :keys [asset] } player
      { :keys [body world] } asset
      mat (phys/get-matrix body)
      body-mat (math/mat4f mat)
      rel (->> (math/vec3f 0 5 0) (.transformPoint body-mat) math/floats-from-vec3f)
      vel (phys/get-point-velocity body rel)
      force (->> vel (mapv -) (mapv (partial * 1/10)))
      f (/ Math/PI 6)
      b (- f)
      wings (concat
        (mapv #(hash-map
            :area 1/2
            :rel [0 5 0]
            :norm [0 -1 0]
            :rot %
            :onesided true
          )
          [
            [0 0 0]
            [f 0 0]
            [b 0 0]
            [0 0 f]
            [0 0 b]
          ]
        )
        [
          {
            :area 1/4
            :rel [0 0 -1]
            :norm [1 0 0]
            :rot [0 0 0]
          }
        ]
      )
      grounded? (on-ground? player)
    ]
    (doseq [w wings] (proc-wing body body-mat w))
    (phys/apply-world-force body force rel)
    (cond
      (= (in :action) :jump) (fall-player asset)
      grounded? (walk-player asset)
      :else (assoc player :mat mat)
    )
  )
)

(defn parachute-player-render [player dev res time]
  (let [
      { :keys [player-model] } res
      { :keys [mat color anim] } player
      offset (mapv - player-offset)
      model (make-player-model (dev :gl) player-model color)
      anim (-> res :anims (get anim) :anim (.animate time))
      parachute (res :parachute)
    ]
    (graph/push-matrix)
    (-> mat math/mat4f graph/apply-matrix)
    (graph/model parachute)
    (apply graph/translate offset)
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
  )
)

(def player-map {
    :walk {
      :next walk-player-next
      :render move-player-render
      :cam next-walk-camera
      :lerp move-player-lerp
    }
    :fly {
      :next fly-player-next
      :render fly-player-render
      :cam next-walk-camera
      :lerp fly-player-lerp
    }
    :parachute {
      :next parachute-player-next
      :render parachute-player-render
      :cam next-parachute-camera
      :lerp mat-player-lerp
    }
    :run {
      :next run-player-next
      :render move-player-render
      :cam next-run-camera
      :lerp move-player-lerp
    }
    :jump {
      :next jump-player-next
      :render jump-player-render
      :cam next-run-camera
      :lerp jump-player-lerp
    }
    :fall {
      :next fall-player-next
      :render mat-player-render
      :cam next-run-camera
      :lerp mat-player-lerp
    }
    :lost {
      :next lost-player-next
      :render mat-player-render
      :cam next-run-camera
      :lerp mat-player-lerp
    }
    :wallrun {
      :next wallrun-player-next
      :render mat-player-render
      :cam next-run-camera
      :lerp mat-player-lerp
    }
    :flip {
      :next flip-player-next
      :render flip-player-render
      :cam next-run-camera
      :lerp mat-player-lerp
    }
  }
)

(defn next-player [p in time delta-time]
  (-> p :state player-map :next (funcall p in time delta-time)
    ((-> p :state player-map :cam) time delta-time)
  )
)

(defn render-player [p dev res time]
  (-> p :state player-map :render (funcall p dev res time))
)

(defn lerp-player [a b t]
  (cond
    (= (a :state) (b :state)) (-> a :state player-map :lerp (funcall a b t))
    :else b
  )
)

(defn game-setup [dev res level-name colors]
  (let [
      world (phys/dynamics-world)
      level (-> res :levels level-name)
      { :keys [markers model shapes] } level
      start (markers "Start")
      spawn-pos (-> start (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
      spawn-look (-> start (.transformVector (math/vec3f 0 0 1)) math/floats-from-vec3f)
      spawn-rot (math/look-rot spawn-look)
      spawn-pos-a (mapv + [2 0 0] spawn-pos)
      players (mapv
        (fn [pos control color]
          (walk-player (player-asset world pos spawn-rot control color))
        )
        [spawn-pos spawn-pos-a]
        [:left :right]
        colors
      )
    ]
    (doseq [s shapes] (phys/add-rigid-body world (s :shape) [0 0 0] [0 0 0] 0))
    {
      :loop game-loop
      :exit (constantly nil)
      :players players
      :player-num (count colors)
      :time (--> dev :get-time ())
      :model model
      :world world
    }
  )
)

(defn make-netplayer [player]
  (assoc (dissoc player :asset) :world-center (-> player :asset :body phys/get-position))
)

(defn client-loop [dev res state]
  (cond (client/running?)
    (let [
        net-players (->> (client/got-lerp lerp-player) vals (apply concat))
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

(defn client-setup [dev res level-name addr color name]
  (client/start-client addr (res :net-port) (res :udp-port) name)
  (assoc (game-setup dev res level-name [color])
    :loop client-loop
    :exit client/close-client
  )
)

(defn server-setup [dev res level-name color name]
  (server/start-server (res :net-port) (res :udp-port) (res :broadcast-port)
    { :name name :level level-name }
  )
  (assoc (client-setup dev res level-name "localhost" color name)
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
      run (controls keyboard :run)
      spec (controls keyboard :spec)
      in (cond
        jump (assoc in :action :jump)
        run (assoc in :action :run)
        spec (assoc in :action :spec)
        :else in
      )
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
      (doseq [np net-players]
        (let [
            cen (-> p :asset :body phys/get-position)
            pos (np :world-center)
            dir (mapv - pos cen)
            color (np :color)
            arrow (graph/replace-materials gl (res :arrow) { "Color-material" color })
          ]
          (when (< 2 (mat/length dir))
            (graph/push-matrix)
            (apply graph/translate cen)
            (apply graph/rotate (-> dir math/normalize math/look-rot))
            (graph/translate 0 0 2/3)
            (graph/scale 1/3 1/3 1/3)
            (graph/model arrow)
            (graph/pop-matrix)
          )
        )
      )
    ) players (-> player-num viewports cycle))
    (graph/viewport gl 0 0 w h)
  )
)
