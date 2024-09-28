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
  (mapv
    (partial load-preset gl diffuse-shader skin-shader)
    ((comp vec pmap)
      (partial read-preset storage)
      (keys char-presets)
    )
  )
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

(defn next-char [ch in] (char-call ch :next in))

(defn update-char [ch in] (char-call ch :update in))

(defn render-char [ch] (char-call ch :render))

; state scheme
; {
;   :update (self in -> ())
;   :effect (self in physics -> [effects])
;   :next (self in effect -> next-state)
;   :render (self -> ())
; }

(defn new-state
  ([anim timer rtimer next] (new-state anim timer rtimer next (constantly ())))
  ([anim timer rtimer next update] (new-state anim timer rtimer next update (constantly [])))
  ([anim timer rtimer next update effect]
    {
      :anim anim
      :start (timer)
      :timer timer
      :rtimer rtimer
      :next next
      :update update
      :effect effect
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

(defn render-item [size item]
  (graph/push-matrix)
  (apply graph/scale size)
  (graph/apply-matrix (-> :body item phys/get-matrix math/mat4f))
  (graph/model (item :model))
  (graph/pop-matrix)
)

(defn arrow [phys-world owner pos rot model]
  {
    :model model
    :body (phys/capsule phys-world pos (mapv + [(/ Math/PI -2) 0 0] rot) 0.2 1.2 1)
    :render (partial render-item [1/4 1/4 1/4])
    :next identity
    :has-hit (atom false)
    :effect (fn [item in phys]
      (let [
          c (get (phys :contacts) (item :body) ())
        ]
        (cond
          (or (= c ()) (= c owner)) []
          :else [ (damage-effect c (item :has-hit) 100) ]
        )
      )
    )
  }
)

(defn fireball [phys-world owner pos rot model]
  {
    :model model
    :body (doto
      (phys/sphere phys-world pos rot 0.5 1)
      (phys/set-gravity [0 0 0])
    )
    :next identity
    :render (partial render-item [1/2 1/2 1/2])
    :effect (fn [item in phys]
      (let [c (get (phys :contacts) (item :body) ())]
        (cond
          (= c owner) []
          (= c ()) []
          :else [(remove-item-effect item) (damage-effect c 100)]
        )
      )
    )
  }
)

(defn state-age [s]
  (- ((s :timer)) (s :start))
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

(defn ai-in [ch chs]
  (let [
      { :keys [ pos target ] } ch
      tdir (cond
        (= target ()) [0 0 0]
        :else (mapv - (phys/get-position target) pos)
      )
      [mx my mz] (math/normalize tdir)
    ]
    (cond
      (= target ()) (move-in [0 0 0])
      (->> tdir mat/length (> 2)) (look-action-in tdir :attack)
      :else (move-in [mx 0 mz])
    )
  )
)

(defn ai-next [chs body-to-char ch]
  (let [ next (char-call ch :next (ai-in ch chs)) ]
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

(defn idle-state [timer rtimer]
  (new-state "idle" timer rtimer (fn [s ch in]
      (let [
          { :keys [movement action] } in
        ]
        (cond
          (= action :attack) (map-state ch :attack timer rtimer)
          (math/zero-len? movement) (next-char-mov ch in)
          :else (map-state ch :walk timer rtimer)
        )
      )
    )
    (fn [s ch in] (move-char ch [0 0 0]))
  )
)

(defn walk-state [timer rtimer]
  (new-state "walk" timer rtimer (fn [s ch in]
      (let [
          { :keys [movement action] } in
        ]
        (cond
          (= action :attack) (map-state ch :attack timer rtimer)
          (math/zero-len? movement) (map-state ch :idle timer rtimer)
          :else (next-char-mov ch in)
        )
      )
    )
    (fn [s ch in]
      (let [
          { :keys [movement] } in
        ]
        (move-char ch (mapv (partial * 6) movement))
        ()
      )
    )
  )
)

(defn attack-state [attack-anims timer rtimer]
  (let [
      anim (attack-anims (rand-int (count attack-anims)))
    ]
    (new-state anim timer rtimer
      (fn [s ch in]
        (cond
          (< (char-anim-length ch anim) (state-age s)) (map-state ch :idle timer rtimer)
          :else (next-char-mov ch in)
        )
      )
      (fn [s ch in] (move-char ch [0 0 0]))
    )
  )
)

(defn projectile-attack-state [attack-anim projectile-spawn spawn-time spawn-force timer rtimer]
  (let [
      atk (attack-state [attack-anim] timer rtimer)
      effect-fn
      (fn [s ch in phys]
        (cond
          (or (-> s :has-arrow deref true?) (-> s state-age (< spawn-time))) []
          :else (let [
              { :keys [look pos body world] } ch
              [lx ly lz] look
              arr-pos (mapv + pos (mapv * look (repeat 2)) [0 2 0])
              arr-rot [0 (math/clock lx lz) 0]
              arr (projectile-spawn world body arr-pos arr-rot (-> ch :items first))
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

(defn melee-attack-state [anims timer rtimer]
  (let [
      atk (attack-state anims timer rtimer)
      eff-fn (fn [s ch in phys]
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

(defn death-state [timer rtimer]
  (assoc (new-state "death" timer rtimer (fn [s ch in]
      (cond
        (>= (state-age s) (- (char-anim-length ch "death") 0.1)) (map-state ch :dead timer rtimer)
        :else (next-char-idle ch)
      )
    )
    (fn [s ch in] (when
        (-> s :disposed deref false?)
        (.removeRigidBody (ch :world) (ch :body))
        (-> s :disposed (reset! true))
      )
    )
  ) :disposed (atom false))
)
(defn dead-state [timer rtimer]
  (new-state "dead" timer rtimer (fn [s ch in] (next-char-idle ch)))
)

(defn wrap-mortal [factory]
  (fn [timer rtimer]
    (let [state (factory timer rtimer)]
      (assoc state :next
        (fn [s ch in]
          (cond
            (<= (ch :health) 0) (map-state ch :death timer rtimer)
            :else ((state :next) s ch in)
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
      :attack (wrap-mortal (partial projectile-attack-state "attack" arrow 1.2 100))
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

(defn class-state [name sym timer rtimer]
  ((-> states-map name sym) timer rtimer)
)

(defn map-state [ch sym timer rtimer]
  (assoc ch :state (class-state (ch :name) sym timer rtimer))
)

(defn load-char [world preset pos look color side timer]
  {
    :target ()
    :side side
    :health 100
    :world world
    :items (preset :items)
    :name (preset :name)
    :anims (preset :anims)
    :materials ((preset :materials-loader) (-> preset :model :materials) color)
    :body (-> world
      (phys/capsule (mapv + [0 1 0] pos) [0 0 0] 0.25 3/2 1)
      (phys/set-rotation-enabled false)
    )
    :pos pos
    :look look
    :state (class-state (preset :name) :idle timer timer)
    :next (fn [ch in]
      (let [
          state (ch :state)
          next (state :next)
        ]
        (next state ch in)
      )
    )
    :update (fn [ch in]
      (char-call (ch :state) :update ch in)
    )
    :render (fn [ch]
      (let [
          { :keys [state look pos materials] } ch
          { :keys [anim start rtimer] } state
          [lx ly lz] look
        ]
        (graph/push-matrix)
        (graph/apply-matrix (math/rotation 0 (math/clock lx lz) 0))
        (apply graph/translate pos)
        (render-preset preset materials anim (- (rtimer) start))
        (graph/pop-matrix)
        ()
      )
    )
    :effect (fn [ch in phys]
      (-> ch :state (char-call :effect ch in phys))
    )
  }
)

(defn update-game-state [dev state]
  (let [
      { :keys [player non-players movement action] } state
      all (cons player non-players)
    ]
    (update-char player (move-action-in movement action))
    (doseq [n non-players] (update-char n (ai-in n all)))
  )
)

(def cam+ 4)
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
      { :keys [camrot campos player non-players items phys-world] } state
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
        (keyboard "f" :down) :attack
        :else :none
      )

      contacts (phys/get-all-contacts phys-world)
      phys { :contacts contacts }
      in (move-action-in movement action)
      effects (apply concat (char-list-call (concat [player] non-players items) :effect in phys))
      [effect global-effect] (multi-effect effects)
      all-players (cons player non-players)
      body-to-char (zipmap (map :body all-players) all-players)
      player (next-char player in)
      player (effect player)
      non-players (mapv (partial ai-next all-players body-to-char) non-players)
      non-players (mapv effect non-players)
      items (char-list-call items :next)

      playerpos (player :pos)
      camsub (mapv - campos playerpos)
      camsub (update camsub 1 (constantly 0))
      camsub (if (zero? (mat/length camsub)) [0 0 -1] (mat/normalise camsub))

      ray-origin (mapv + [0 cam+ 0] playerpos)
      { :keys [has-hit dist normal] } (phys/ray-check phys-world ray-origin camsub camdist)
      [camsub camdist]
      (if has-hit
        [camsub dist]
        [camsub camdist]
      )

      [cx cy cz] (mapv - camsub)
      camrot [camrotx+ (math/clock cx cz) 0]
      campos (mapv + playerpos (mapv + [0 cam+ 0] (mapv * camsub (repeat camdist))))


      [player non-players campos camrot]
      (cond (keyboard "c" :up)
        (let [
          p (first non-players)
          n (conj ((comp vec rest) non-players) player)
          [cpos crot] (player-cam p)
        ] [p n cpos crot])
        :else [player non-players campos camrot]
      )

      state (global-effect (assoc state
        :action action
        :campos campos
        :camrot camrot
        :movement movement
        :player player
        :items items
        :non-players non-players
      ))
    ]
    state
  )
)