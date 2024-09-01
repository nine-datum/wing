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

(defn preset [model-name offset-geom-name items & anims]
  {
    :model-name model-name
    :offset-geom offset-geom-name
    :anims anims
    :items items
  }
)

(def char-presets
  {
    :archer (preset "archer" "Cube_001-mesh" [ "res/datum/arrow.dae" ]
      "attack"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :fighter (preset "fighter" "Cube_002-mesh" []
      "attack"
      "attack_2"
      "block"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :mage (preset "mage" "Cube_002-mesh" []
      "attackspell"
      "spherespell"
      "teleportspell"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :ninja (preset "ninja" "Cube_003-mesh" []
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
      items-loader (fn [gl diffuse-shader skin-shader]
        (mapv (partial load-item-model gl storage diffuse-shader skin-shader) items)
      )
    ] {
      :name key
      :loader loader
      :items-loader items-loader
      :anims anims
    }
  )
)

(defn load-preset [gl diffuse-shader skin-shader preset]
  {
    :name (preset :name)
    :model ((preset :loader) gl diffuse-shader skin-shader)
    :items ((preset :items-loader) gl diffuse-shader skin-shader)
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
      [skin-anim obj-anim] (mapv :anim (anims anim))
      skin-anim (graph/animate skin-anim time)
      obj-anim (graph/animate obj-anim time)
    ]
    (graph/animated-model model skin-anim obj-anim)
  )
)

(defn char-call [ch sym & args]
  (apply (ch sym) (cons ch args))
)

(defn next-char [ch in eff] (char-call ch :next in eff))

(defn update-char [ch in] (char-call ch :update in))

(defn render-char [ch] (char-call ch :render))

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

(defn damage-effect [body damage]
  [
    (fn [ch] (-> ch :body (= body)))
    (fn [ch] (update ch :health #(- % damage)))
  ]
)

(defn arrow [phys-world pos rot model]
  {
    :body (phys/capsule phys-world pos (mapv + [(/ Math/PI -2) 0 0] rot) 0.2 1.2 1)
    :render (fn [item]
      (graph/push-matrix)
      (graph/apply-matrix (-> :body item phys/get-matrix math/mat4f))
      (graph/model model)
      (graph/pop-matrix)
    )
    :effect (fn [item ch phys]
      (let [
          c (get (phys :contacts) (item :body) ())
        ]
        (cond
          (or (= c ()) (= c (ch :body))) []
          :else [ (damage-effect c 100) ]
        )
      )
    )
  }
)

(defn state-age [s]
  (- ((s :timer)) (s :start))
)

(defn char-anim [ch name]
  ((ch :anims) name)
)

(defn char-anim-length [s name]
  ((first (char-anim s name)) :length)
)

(defn move-char [ch mvec]
  (phys/move-char (ch :body) mvec)
)

(defn add-item [ch item]
  (update ch :items (partial cons item))
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
      { :keys [body pos look] } ch
      { :keys [movement] } in
      look (cond
        (math/zero-len? movement) look
        :else (math/normalize-checked movement)
      )
    ]
    (assoc (next-char-idle ch) :look look)
  )
)

(declare map-state)
(declare class-state)

(defn idle-state [timer rtimer]
  (new-state "idle" timer rtimer (fn [s ch in eff]
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
  (new-state "walk" timer rtimer (fn [s ch in eff]
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
      (fn [s ch in eff]
        (cond
          (< (char-anim-length ch anim) (state-age s)) (map-state ch :idle timer rtimer)
          :else (next-char-idle ch)
        )
      )
      (fn [s ch in] (move-char ch [0 0 0]))
    )
  )
)

(defn archer-attack-state [timer rtimer]
  (let [
      atk (attack-state ["attack"] timer rtimer)
      next-fn
      (fn [s ch in eff]
        (cond
          (or (s :has-arrow) (-> s state-age (< 1.2))) ((atk :next) s ch in eff)
          :else (let [
              { :keys [look pos] } ch
              [lx ly lz] look
              arr-pos (mapv + pos look [0 2 0])
              arr-rot [0 (math/clock lx lz) 0]
              arr (arrow (ch :world) arr-pos arr-rot (-> ch :item-models first))
            ]
            (phys/set-velocity (arr :body) (mapv * look (repeat 100)))
            (update
              (add-item ch arr)
              :state
              #(assoc % :has-arrow true)
            )
          )
        )
      )
      state (assoc atk :next next-fn :has-arrow false)
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
            ctr (mapv + pos look [0 1 0])
            cs (phys/sphere-check (ch :world) ctr (mapv + ctr look) 0.5)
            cs (disj (set cs) body)
          ]
          (mapv damage-effect cs (repeat 100))
        )
      )
    ]
    (assoc atk :effect eff-fn :has-shot false)
  )
)

(defn death-state [timer rtimer]
  (new-state "death" timer rtimer (fn [s ch in eff]
    (cond
      (>= (state-age s) (- (char-anim-length ch "death") 0.1)) (map-state ch :dead timer rtimer)
      :else (next-char-idle ch)
    )
  )
  (fn [s ch in] (move-char ch [0 0 0])))
)
(defn dead-state [timer rtimer]
  (new-state "dead" timer rtimer (fn [s ch in eff] (next-char-idle ch)))
)

(defn wrap-mortal [factory]
  (fn [timer rtimer]
    (let [state (factory timer rtimer)]
      (assoc state :next
        (fn [s ch in eff]
          (cond
            (<= (ch :health) 0) (map-state ch :death timer rtimer)
            :else ((state :next) s ch in eff)
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
      :attack (wrap-mortal archer-attack-state)
    )
    :fighter (assoc base-state
      :attack (wrap-mortal (partial melee-attack-state ["attack" "attack_2"]))
    )
    :mage (assoc base-state
      :attack (wrap-mortal (partial attack-state ["attackspell"]))
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

(defn render-item [item]
  (char-call item :render)
)

(defn load-char [world preset pos look timer]
  {
    :health 100
    :world world
    :item-models (preset :items)
    :items ()
    :name (preset :name)
    :anims (preset :anims)
    :body (-> world
      (phys/capsule (mapv + [0 3/4 0] pos) [0 0 0] 0.25 3/2 1)
      (phys/set-rotation-enabled false)
    )
    :pos pos
    :look look
    :state (class-state (preset :name) :idle timer timer)
    :next (fn [ch in eff]
      (let [
          { :keys [state body] } ch
          next (state :next)
          effs (filter (comp #(% ch) first) eff)
          ch (reduce #((second %2) %1) ch effs)
        ]
        (next state ch in eff)
      )
    )
    :update (fn [ch in]
      (char-call (ch :state) :update ch in)
    )
    :render (fn [ch]
      (let [
          { :keys [state look pos items] } ch
          { :keys [anim start rtimer] } state
          [lx ly lz] look
        ]
        (graph/push-matrix)
        (graph/apply-matrix (math/rotation 0 (math/clock lx lz) 0))
        (apply graph/translate pos)
        (render-preset preset anim (- (rtimer) start))
        (graph/pop-matrix)
        (doseq [item items] (render-item item))
        ()
      )
    )
    :effect (fn [ch in phys]
      (concat
        (-> ch :state (char-call :effect ch in phys))
        (->> ch :items ((comp (partial apply concat) map) #(char-call % :effect ch phys)))
      )
    )
  }
)

(defn update-game-state [dev state]
  (update-char (state :player) { :movement (state :movement) :action (state :action) })
  (doseq [n (state :non-players)] (update-char n { :movement [0 0 0] }))
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
      { :keys [camrot campos player non-players phys-world] } state
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
      in { :movement movement :action action }
      effects ((comp (partial apply concat) map) #(char-call % :effect in phys) (cons player non-players))
      player (next-char player in effects)
      non-players (mapv next-char non-players (repeat { :movement [0 0 0] :action :none }) (repeat effects))

      playerpos (player :pos)
      camsub (mapv - campos playerpos)
      camsub (update camsub 1 (constantly cam+))
      camsub (if (zero? (mat/length camsub)) [0 1 -1] (mat/normalise camsub))
      [cx cy cz] (mapv - camsub)
      camrot [(- camrotx+) (math/clock cx cz) 0]
      campos (mapv + playerpos (mapv * camsub (repeat camdist)))


      [player non-players campos camrot]
      (cond (keyboard "c" :up)
        (let [
          p (first non-players)
          n (conj ((comp vec rest) non-players) player)
          [cpos crot] (player-cam p)
        ] [p n cpos crot])
        :else [player non-players campos camrot]
      )

      state (assoc state
        :action action
        :campos campos
        :camrot camrot
        :movement movement
        :player player
        :non-players non-players
      )
    ]
    state
  )
)