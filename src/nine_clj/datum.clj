(ns nine-clj.datum
  [:require
    [nine-clj.graph :as graph]
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
      (graph/geom-offset-parser (partial contains? (apply hash-set offset-geom-names)) [0 0 1])
      (ColladaBasicSkinParser.)
      (ColladaBasicAnimationParser.)
      (ColladaBasicMaterialParser.)
    )
    file
  )
)

(defn load-model [gl storage diffuse-shader skin-shader name offset-geom]
  (load-offset-animated-model gl storage diffuse-shader skin-shader (format "res/datum/%s.dae" name) offset-geom)
)

(defn load-anim [storage model-name anim-name]
  (let [
      af (format "res/datum/anims/%s/%s.clj" model-name anim-name)
      mf (format "res/datum/%s.dae" model-name)
    ]
    (vector
      (graph/load-anim-clj storage (partial = "JOINT") af mf)
      (graph/load-anim-clj storage (partial = "NODE") af mf)
    )
  )
)

(defn preset [model-name offset-geom-name & anims]
  {
    :model-name model-name
    :offset-geom offset-geom-name
    :anims anims
  }
)

(def presets
  {
    :archer (preset "archer" "Cube_001-mesh"
      "attack"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :fighter (preset "fighter" "Cube_002-mesh"
      "attack"
      "attack_2"
      "block"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :mage (preset "mage" "Cube_002-mesh"
      "attackspeell"
      "spherespell"
      "teleportspell"
      "idle"
      "walk"
      "death"
      "dead"
    )
    :ninja (preset "ninja" "Cube_003-mesh"
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

(defn load-preset [gl storage diffuse-shader skin-shader key]
  (let
    [
      { :keys [
          model-name
          offset-geom
          anims
        ]
      } (presets key)
      model (load-model gl storage diffuse-shader skin-shader model-name offset-geom)
      anims (
        (comp
          (partial apply hash-map)
          (partial apply concat)
          (partial map vector anims)
          (partial map (partial load-anim storage model-name))
        )
        anims
      )
    ] {
      :model model
      :anims anims
    }
  )
)

(defn update-player-state [dev state]
  (let [
      { :keys [camrot look] } state
      { :keys [keyboard mouse] } dev
      [camx camy camz] camrot
      [mousex mousey] (mapv (partial * 0.01) (mouse :delta))
      camrot [(- camx mousey) (+ camy mousex) camz]
      [camx camy camz] camrot
      hpi (/ Math/PI 2)
      camx (->> camx (max (- hpi)) (min hpi))
      camrot [ camx camy camz ]
      [wasd-x wasd-y] (input/wasd keyboard)
      cammat (apply math/rotation camrot)
      cam-fwd (math/get-column-3 cammat 2)
      cam-right (math/get-column-3 cammat 0)
      cam-fwd (mapv (partial * wasd-y) cam-fwd)
      cam-right (mapv (partial * wasd-x) cam-right)
      [mov-x mov-y mov-z] (mapv + cam-fwd cam-right)
      movement ((comp (partial mapv math/escape-nan) mat/normalise) [mov-x 0 mov-z])
      look (if (zero? (mat/length movement)) look movement)
      state (assoc state :camrot camrot :movement movement :look look)
    ]
    state
  )
)

(defn render-preset [preset anim time]
  (let [
      { :keys [model anims] } preset
      [skin-anim obj-anim] (anims anim)
      skin-anim (graph/animate skin-anim time)
      obj-anim (graph/animate obj-anim time)
    ]
    (graph/animated-model model skin-anim obj-anim)
  )
)