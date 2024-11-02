(ns nine-clj.datum
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.phys :as phys]
    [nine-clj.input :as input]
    [nine-clj.prof :as prof]
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

(defn load-item-model [gl storage diffuse-shader skin-shader file]
  (graph/load-model
    (graph/load-graphics gl storage diffuse-shader skin-shader)
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

(defn preset [model-name offset-geom-name items materials paint-materials & anims]
  {
    :model-name model-name
    :offset-geom offset-geom-name
    :anims anims
    :items items
    :materials materials
    :paint-materials paint-materials
  }
)

(def char-presets
  {
    :archer (preset "archer" "Cube_001-mesh" [ "res/datum/arrow.dae" ]
      {}
      [ "Odezhda_tsvet-material" ]
      "attack"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "armature|riding"
    )
    :fighter (preset "fighter" "Cube_002-mesh" []
      {}
      [ "Metall_color-material" ]
      "attack"
      "attack_2"
      "block"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "armature|riding"
    )
    :mage (preset "mage" "Cube_002-mesh" ["res/datum/fireball.dae"]
      {}
      [ "Odezhda_tsvet_001-material" ]
      "attackspell"
      "spherespell"
      "teleportspell"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "armature|riding"
    )
    :ninja (preset "ninja" "Cube_003-mesh" []
      {}
      [ "Odezhda_tsvet-material" ]
      "attack"
      "attack_2"
      "attack_3"
      "idle"
      "walk"
      "death"
      "dead"
      "idle_pass"
      "walk_pass"
      "armature|riding"
    )
  }
)

(defn stats [walk-speed walk-anim-speed attack-damage health]
  {
    :walk-speed walk-speed
    :walk-anim-speed walk-anim-speed
    :attack-damage attack-damage
    :health health
  }
)

(def char-stats {
    :archer (stats 8 1 50 100)
    :fighter (stats 5 0.75 25 150)
    :ninja (stats 10 1.2 35 80)
    :mage (stats 7 1 75 80)
  }
)

(defn char-anim-speed [ch]
  (-> ch :state :speed (apply [ch]))
)

(defn get-char-stat [ch stat] (-> ch :name char-stats stat))

(defn read-preset [storage key]
  (let
    [
      { :keys [
          model-name
          offset-geom
          anims
          items
          materials
          paint-materials
        ]
      } (char-presets key)
      loader (fn [gl diffuse-shader skin-shader] (load-model gl storage diffuse-shader skin-shader model-name offset-geom))
      anims (
        (comp
          (partial apply hash-map)
          (partial mapcat vector anims)
          (partial pmap (partial load-anim storage model-name))
        )
        anims
      )
      items-loader (fn [gl diffuse-shader skin-shader]
        (mapv (partial load-item-model gl storage diffuse-shader skin-shader) items)
      )
    ] {
      :name key
      :loader loader
      :items-loader items-loader
      :anims anims
      :materials materials
      :paint-materials paint-materials
    }
  )
)

(defn load-preset-materials [gl preset base-materials main-color]
  (let [
      { :keys [materials paint-materials] } preset
      paint-map (apply hash-map (interleave paint-materials (repeat main-color)))
      mat (graph/material-provider-colors gl (merge materials paint-map))
      mat (graph/material-provider-combine mat paint-materials base-materials)
    ]
    mat
  )
)

(defn load-preset [gl diffuse-shader skin-shader preset]
  {
    :name (preset :name)
    :model ((preset :loader) gl diffuse-shader skin-shader)
    :items ((preset :items-loader) gl diffuse-shader skin-shader)
    :materials-loader (partial load-preset-materials gl preset)
    :anims (preset :anims)
  }
)

(defn load-presets [gl storage diffuse-shader skin-shader]
  (zipmap (keys char-presets) (mapv
    (partial load-preset gl diffuse-shader skin-shader)
    ((comp vec pmap)
      (partial read-preset storage)
      (keys char-presets)
    )
  ))
)

(defn render-preset [preset materials anim time]
  (let [
      { :keys [model anims] } preset
      [skin-anim obj-anim] (mapv :anim (anims anim))
      skin-anim (graph/animate skin-anim time)
      obj-anim (graph/animate obj-anim time)
    ]
    (graph/animated-model model skin-anim obj-anim materials)
  )
)

(defn char-call [ch sym & args]
  (apply (ch sym) (cons ch args))
)

(defn char-list-call [chs sym & args]
  (apply mapv char-call chs (repeat sym) (map repeat args))
)

(defn next-char [ch in time] (char-call ch :next in time))

(defn update-char [ch in time] (char-call ch :update in time))

(defn render-char [ch time] (char-call ch :render time))

; state scheme
; {
;   :update (self in -> ())
;   :effect (self in physics -> [effects])
;   :next (self in effect -> next-state)
;   :render (self -> ())
; }

(defn new-state
  ([anim time next] (new-state anim time next (constantly ())))
  ([anim time next update] (new-state anim time next update (constantly [])))
  ([anim time next update effect]
    {
      :anim anim
      :start time
      :next next
      :update update
      :effect effect
      :speed (constantly 1)
    }
  )
)

(defn once-hit-check []
  (let [
      hits (atom (hash-set))
    ]
    (fn [b]
      (let [ c (contains? @hits b) ]
        (swap! hits #(conj % b))
        (false? c)
      )
    )
  )
)

(defn damage-effect [body hit-check damage]
  [
    (fn [ch] (cond
      (and (-> ch :body hit-check) (-> ch :body (= body)))
        (update ch :health #(- % damage))
      :else ch
    ))
    identity
  ]
)

(defn spawn-item-effect [item]
  [
    identity
    (fn [s] (update s :items (partial cons item)))
  ]
)

(defn multi-effect [effects] 
  [
    (fn [ch] (reduce #((first %2) %1) ch effects))
    (fn [chs] (reduce #((second %2) %1) chs effects))
  ]
)

(defn remove-item-effect [item]
  [
    identity
    (fn [state]
      (->> state
        :items
        (filter (comp (partial not= (item :body)) :body))
        list*
        (assoc state :items)
      )
    )
  ]
)

(defn render-item [size item time]
  (graph/push-matrix)
  (apply graph/scale size)
  (graph/apply-matrix (-> :body item phys/get-matrix math/mat4f))
  (graph/model (item :model))
  (graph/pop-matrix)
)

(defn arrow [time phys-world owner pos rot model]
  {
    :start time
    :model model
    :body (phys/capsule phys-world pos (mapv + [(/ Math/PI -2) 0 0] rot) 0.2 1.2 1)
    :render (partial render-item [1/4 1/4 1/4])
    :next (fn [s time] s)
    :hit-check (once-hit-check)
    :effect (fn [item in phys time]
      (let [
          c (get (phys :contacts) (item :body) ())
        ]
        (cond
          (->> item :start (- time) (< 2)) [ (remove-item-effect item) ]
          (or (-> phys :body-to-char (contains? c) false?) (= c ()) (= c owner)) []
          :else [ (remove-item-effect item) (damage-effect c (item :hit-check) 50) ]
        )
      )
    )
  }
)

(defn fireball [time phys-world owner pos rot model]
  {
    :model model
    :body (doto
      (phys/sphere phys-world pos rot 0.5 1)
      (phys/set-gravity [0 0 0])
    )
    :next (fn [s time] s)
    :render (partial render-item [1/2 1/2 1/2])
    :effect (fn [item in phys time]
      (let [c (get (phys :contacts) (item :body) ())]
        (cond
          (= c owner) []
          (= c ()) []
          :else [(remove-item-effect item) (damage-effect c (once-hit-check) 100)]
        )
      )
    )
  }
)

(defn state-age [s time]
  (- time (s :start))
)

(defn is-alive [ch] (-> ch :health (> 0)))

(defn char-anim [ch name]
  ((ch :anims) name)
)

(defn char-anim-length [s name]
  ((first (char-anim s name)) :length)
)

(defn move-char [ch mvec]
  (phys/move-char (ch :body) mvec)
)

(defn next-char-idle [ch]
  (let[
      body (ch :body)
      pos (mapv - (phys/get-position body) [0 3/4 0])
    ]
    (assoc ch :pos pos)
  )
)

(defn next-char-mov [ch in]
  (let [
      look (ch :look)
      in-look (in :look)
      look (cond
        (math/zero-len? in-look) look
        :else (math/normalize-checked in-look)
      )
    ]
    (assoc (next-char-idle ch) :look look)
  )
)


(defn move-in [m]
  { :movement m :action :none :look m }
)

(defn look-in [l]
  { :movement [0 0 0] :action :none :look l }
)

(defn move-action-in [m a]
  { :movement m :action a :look m }
)

(defn look-action-in [l a]
 { :movement [0 0 0] :action a :look l }
)

(defn real-look-in [ch delta-time look]
  (cond
    (-> look mat/length zero?) (-> ch :look look-in)
    :else
    (let [
        [cx cy cz] (ch :look)
        [lx ly lz] look
        c-angle (math/clock cx cz)
        l-angle (math/clock lx lz)
        delta (math/angle- l-angle c-angle)
        eps (/ Math/PI 30)
        dir (cond
          (> delta eps) 1
          (< delta (- eps)) -1
          :else delta
        )
        m (* dir Math/PI 4 delta-time)
        [lx lz] (math/clock-xy (+ c-angle m))
      ]
      (look-in (math/normalize [lx ly lz]))
    )
  )
)

(defn real-move-in [ch delta-time mov]
  (let [
      mnorm (math/normalize mov)
      look (->> mnorm (real-look-in ch delta-time) :look)
      d (mat/dot mnorm look)
      d (if (> d 0) d 0)
    ]
    (assoc (move-in (mapv * (repeat d) mov)) :look look)
  )
)

(defn real-move-action-in [ch delta-time mov action]
  (assoc (real-move-in ch delta-time mov) :action action)
)

(defn real-look-action-in [ch delta-time look action]
  (assoc (real-look-in ch delta-time look) :action action)
)

(defn ch-move-in [ch delta-time mov]
  (move-in mov)
)
(defn ch-look-in [ch delta-time look]
  (look-in look)
)
(defn ch-look-action-in [ch delta-time look action]
  (look-action-in look action)
)
(defn ch-move-action-in [ch delta-time mov action]
  (move-action-in mov action)
)

(defn target-dir [ch body-to-char]
  (let [
      { :keys [ pos target ] } ch
      tdir (cond
        (= target ()) [0 0 0]
        :else (mapv - (-> target body-to-char :pos) pos)
      )
    ]
    tdir
  )
)

(defn ai-target [ch chs] ()
  (let [
      { :keys [ side pos ] } ch
      alive (filter is-alive chs)
      enemies (filter (comp (partial not= side) :side) alive)
      grouped (mapv vector (map (comp (partial mapv -) (partial mapv - pos) :pos) enemies) enemies)
      sorted (sort-by (comp mat/length first) grouped)
      [tdir t] (cond (empty? sorted) [[] {}] :else (first sorted))
    ]
    (if (empty? sorted) () (t :body))
  )
)

(defn ai-in-fighter [ch chs body-to-char delta-time]
  (let [
      target (ch :target)
      tdir (target-dir ch body-to-char)
      [mx my mz] (math/normalize tdir)
    ]
    (cond
      (= target ()) (ch-move-in ch delta-time [0 0 0])
      (->> tdir mat/length (> 2)) (ch-look-action-in ch delta-time tdir :attack)
      :else (ch-move-in ch delta-time [mx 0 mz])
    )
  )
)

(defn ai-in-mage [ch chs body-to-char delta-time]
  (let [
      target (ch :target)
      tdir (target-dir ch body-to-char)
      [mx my mz] (math/normalize tdir)
    ]
    (cond
      (= target ()) (ch-move-in ch delta-time [0 0 0])
      (-> tdir mat/length (< 20)) (ch-look-action-in ch delta-time tdir :attack)
      :else (ch-move-in ch delta-time [mx 0 mz])
    )
  )
)

(defn ai-in-archer [ch chs body-to-char delta-time]
  (let [
      target (ch :target)
      tdir (target-dir ch body-to-char)
      [mx my mz] (math/normalize tdir)
    ]
    (cond
      (= target ()) (ch-move-in ch delta-time [0 0 0])
      (-> tdir mat/length (< 20)) (ch-look-action-in ch delta-time tdir :attack)
      :else (ch-move-in ch delta-time [mx 0 mz])
    )
  )
)

(defn ai-in [ch chs body-to-char delta-time]
  ((case (ch :name)
    :fighter ai-in-fighter
    :ninja ai-in-fighter
    :mage ai-in-mage
    :archer ai-in-archer
  ) ch chs body-to-char delta-time)
)

(defn ai-next [chs body-to-char ch time delta-time]
  (let [ next (char-call ch :next (ai-in ch chs body-to-char delta-time) time) ]
    (cond
      (-> ch :target (= ()))
        (assoc next :target (ai-target ch chs))
      (-> ch :target body-to-char is-alive false?)
        (assoc next :target ())
      :else next
    )
  )
)

(declare map-state)
(declare class-state)

(defn idle-state [time]
  (new-state "idle" time (fn [s ch in time]
      (let [
          { :keys [movement action] } in
        ]
        (cond
          (= action :attack) (map-state ch :attack time)
          (math/zero-len? movement) (next-char-mov ch in)
          :else (map-state ch :walk time)
        )
      )
    )
    (fn [s ch in time] (move-char ch [0 0 0]))
  )
)

(defn walk-state [time]
  (assoc (new-state "walk" time (fn [s ch in time]
      (let [
          { :keys [movement action] } in
        ]
        (cond
          (= action :attack) (map-state ch :attack time)
          (math/zero-len? movement) (map-state ch :idle time)
          :else (next-char-mov ch in)
        )
      )
    )
    (fn [s ch in time]
      (let [
          { :keys [movement] } in
        ]
        (move-char ch (mapv (partial * (get-char-stat ch :walk-speed)) movement))
        ()
      )
    )
  ) :speed (fn [ch] (-> ch (get-char-stat :walk-anim-speed))))
)

(defn attack-state [attack-anims time]
  (let [
      anim (attack-anims (rand-int (count attack-anims)))
    ]
    (new-state anim time
      (fn [s ch in time]
        (cond
          (< (char-anim-length ch anim) (state-age s time)) (map-state ch :idle time)
          :else (next-char-mov ch in)
        )
      )
      (fn [s ch in time] (move-char ch [0 0 0]))
    )
  )
)

(defn projectile-attack-state [attack-anim projectile-spawn spawn-time spawn-force time]
  (let [
      atk (attack-state [attack-anim] time)
      effect-fn
      (fn [s ch in phys time]
        (cond
          (or (-> s :has-arrow deref true?) (-> s (state-age time) (< spawn-time))) []
          :else (let [
              { :keys [look pos body world] } ch
              [lx ly lz] look
              arr-pos (mapv + pos (mapv * look (repeat 2)) [0 1.5 0])
              arr-rot [0 (math/clock lx lz) 0]
              arr (projectile-spawn time world body arr-pos arr-rot (-> ch :items first))
            ]
            (phys/set-velocity (arr :body) (mapv * look (repeat spawn-force)))
            (reset! (s :has-arrow) true)
            [(spawn-item-effect arr)]
          )
        )
      )
      state (assoc atk :effect effect-fn :has-arrow (atom false))
    ]
    state
  )
)

(defn melee-attack-state [anims time]
  (let [
      atk (attack-state anims time)
      eff-fn (fn [s ch in phys time]
        (let [
            { :keys [pos look body] } ch
            { :keys [hit-check] } s
            ctr (mapv + pos [0 1 0])
            cs (phys/sphere-check (ch :world) ctr (mapv + ctr (mapv (partial * 2) look)) 0.5)
            cs (disj (set cs) body)
          ]
          (mapv damage-effect cs (repeat hit-check) (repeat 20))
        )
      )
    ]
    (assoc atk :effect eff-fn :hit-check (once-hit-check))
  )
)

(defn death-state [time]
  (assoc (new-state "death" time (fn [s ch in time]
      (cond
        (>= (state-age s time) (- (char-anim-length ch "death") 0.1)) (map-state ch :dead time)
        :else (next-char-idle ch)
      )
    )
    (fn [s ch in time] (when
        (-> s :disposed deref false?)
        (.removeRigidBody (ch :world) (ch :body))
        (-> s :disposed (reset! true))
      )
    )
  ) :disposed (atom false))
)
(defn dead-state [time]
  (new-state "dead" time (fn [s ch in time] (next-char-idle ch)))
)

(defn wrap-mortal [factory]
  (fn [time]
    (let [state (factory time)]
      (assoc state :next
        (fn [s ch in time]
          (cond
            (<= (ch :health) 0) (map-state ch :death time)
            :else ((state :next) s ch in time)
          )
        )
      )
    )
  )
)

(def base-state {
    :idle (wrap-mortal idle-state)
    :walk (wrap-mortal walk-state)
    :death death-state
    :dead dead-state
  }
)

(def states-map {
    :archer (assoc base-state
      :attack (wrap-mortal (partial projectile-attack-state "attack" arrow 1.2 40))
    )
    :fighter (assoc base-state
      :attack (wrap-mortal (partial melee-attack-state ["attack" "attack_2"]))
    )
    :mage (assoc base-state
      :attack (wrap-mortal (partial projectile-attack-state "attackspell" fireball 1 10))
    )
    :ninja (assoc base-state
      :attack (wrap-mortal (partial melee-attack-state ["attack" "attack_2" "attack_3"]))
    )
  }
)

(defn class-state [name sym time]
  ((-> states-map name sym) time)
)

(defn map-state [ch sym time]
  (assoc ch :state (class-state (ch :name) sym time))
)

(defn load-char-materials [preset color]
  ((preset :materials-loader) (-> preset :model :materials) color)
)

(defn load-char [world preset pos look color side time]
  {
    :target ()
    :side side
    :health (-> preset :name char-stats :health)
    :world world
    :items (preset :items)
    :name (preset :name)
    :anims (preset :anims)
    :materials (load-char-materials preset color)
    :body (-> world
      (phys/capsule (mapv + [0 1 0] pos) [0 0 0] 0.25 3/2 1)
      (phys/set-rotation-enabled false)
    )
    :pos pos
    :look look
    :state (class-state (preset :name) :idle time)
    :next (fn [ch in time]
      (let [
          state (ch :state)
          next (state :next)
        ]
        (next state ch in time)
      )
    )
    :update (fn [ch in time]
      (char-call (ch :state) :update ch in time)
    )
    :render (fn [ch time]
      (let [
          { :keys [state look pos materials] } ch
          { :keys [anim start] } state
          [lx ly lz] look
          anim-speed (char-anim-speed ch)
        ]
        (graph/push-matrix)
        (graph/apply-matrix (math/rotation 0 (math/clock lx lz) 0))
        (apply graph/translate pos)
        (render-preset preset materials anim (-> time (- start) (* anim-speed)))
        (graph/pop-matrix)
        ()
      )
    )
    :effect (fn [ch in phys time]
      (-> ch :state (char-call :effect ch in phys time))
    )
  }
)

(defn update-game-state [dev state]
  (let [
      { :keys [player non-players movement action time delta-time] } state
      all (cons player non-players)
      body-to-char (zipmap (map :body all) all)
    ]
    (update-char player (ch-move-action-in player delta-time movement action) time)
    (doseq [n non-players] (update-char n (ai-in n all body-to-char delta-time) time))
  )
)

(def cam+ 2)
(def camdist 8)
(def camrotx+ (/ Math/PI 12))

(defn player-cam [p]
  (let [
    look (p :look)
    pos (p :pos)
    [lx ly lz] look
  ] [
      (mapv + pos [0 cam+ 0] (mapv * look (repeat (- camdist))))
      [camrotx+ (math/clock lx lz) 0]
    ]
  )
)

(defn next-game-state [dev state]
  (let [
      { :keys [camrot campos player non-players items phys-world time delta-time] } state
      { :keys [keyboard mouse] } dev
      
      cammat (apply math/rotation camrot)
      cam-fwd (math/get-column-3 cammat 2)
      cam-right (math/get-column-3 cammat 0)
      
      [wasd-x wasd-y] (input/wasd keyboard)
      cam-fwd (mapv (partial * wasd-y) cam-fwd)
      cam-right (mapv (partial * wasd-x) cam-right)
      [mov-x mov-y mov-z] (mapv + cam-fwd cam-right)
      movement (math/normalize [mov-x 0 mov-z])

      action (cond
        (input/key-down keyboard \f) :attack
        :else :none
      )

      contacts (phys/get-all-contacts phys-world)
      all-players (cons player non-players)
      body-to-char (zipmap (map :body all-players) all-players)
      phys { :contacts contacts :body-to-char body-to-char }
      in (ch-move-action-in player delta-time movement action)
      effects (apply concat (char-list-call (concat [player] non-players items) :effect in phys time))
      [effect global-effect] (multi-effect effects)
      player (next-char player in time)
      player (effect player)
      non-players (mapv (partial ai-next all-players body-to-char) non-players (repeat time) (repeat delta-time))
      non-players (mapv effect non-players)
      items (char-list-call items :next time)

      [player non-players]
      (cond (or (-> player is-alive false?) (input/key-up keyboard \c))
        (let [
            ps (zipmap (range) non-players)
            player-pred (every-pred is-alive #(-> % :side (= (player :side))))
            [i p] ((comp first filter) (comp player-pred second) (sort-by first ps))
          ]
          (cond
            (= p nil) [player non-players]
            :else
            (let [
                ps (->> (dissoc ps i) (sort-by first) (map second) vec)
                n (conj ps player)
              ] [p n]
            )
          )
        )
        :else [player non-players]
      )

      playerpos (player :pos)
      camsub (mapv - campos playerpos)
      camsub (update camsub 1 (constantly 0))
      camsub (if (zero? (mat/length camsub)) [0 0 -1] (mat/normalise camsub))

      [cx cy cz] (mapv - camsub)
      camrot [camrotx+ (math/clock cx cz) 0]
      [arrows-x arrows-y] (input/arrows keyboard)
      arrows-rot [(-> arrows-y - (* Math/PI 1/4)) (* arrows-x Math/PI 10 delta-time) 0]
      camrot (mapv + camrot arrows-rot)
      cammat (apply math/rotation camrot)
      campiv (mapv + playerpos [0 cam+ 0])
      camdir (->> camdist - (math/vec3f 0 0) (.transformVector cammat) math/floats-from-vec3f)

      ray-origin campiv
      ray-len (mat/length camdir)
      { :keys [has-hit dist normal] } (phys/ray-check phys-world ray-origin camdir ray-len)
      camdist (if has-hit dist ray-len)
      camdir (mapv (partial * camdist) (math/normalize camdir))
      campos (mapv + campiv camdir)

      state (global-effect (assoc state
        :action action
        :campos (mat/lerp (state :campos) campos (* 5 delta-time))
        :camrot (math/lerpv-angle (state :camrot) camrot (* 10 delta-time))
        :movement movement
        :player player
        :items items
        :non-players non-players
      ))
    ]
    state
  )
)