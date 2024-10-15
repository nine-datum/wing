(ns nine-clj.graph
  [:require
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.prof :as prof]
  ]
  [:import
    [nine.lwjgl
      LWJGL_OpenGL
    ]
    [nine.drawing
      Color
    ]
    [nine.opengl
      Shader
      Drawing
      Texture
      Profiler
    ]
    [nine.function
      Condition
      RefreshStatus
    ]
    [nine.game
      Graphics
    ]
    [nine.buffer
      Buffer
      CachedBuffer
    ]
    [nine.geometry
      Skeleton
      AnimatedSkeleton
      AnimatedSkeletonSource
      Animation
      Animator
      KeyFrameAnimation
      Material
      MaterialProvider
    ]
    [nine.geometry.collada
      NoAnimationParser
      ColladaBasicAnimationParser
      ColladaBasicSkeletonParser
      ColladaBasicSkinParser
      ColladaBasicGeometryParser
      ColladaBasicMaterialParser
      ColladaAnimationReader
      ColladaAnimationParser
      ColladaGeometryParser
      ColladaNode
      BuffersReader
      BufferMapping
    ]
  ]
)

(def nine-profiler
  (proxy [Profiler][]
    (profile [name func] (prof/profile name (.call func)))
  )
)

(defn new-gl [] (LWJGL_OpenGL.))

(def matrix-stack (atom (list (math/mat-identity))))
(def projection-matrix (atom (math/mat-identity)))
(def camera-matrix (atom (math/mat-identity)))
(def world-light-vec (atom (math/vec3f 0 0 1)))

(defn projection [p]
  (reset! projection-matrix p)
)

(defn get-projection [] @projection-matrix)

(defn camera [c]
  (reset! camera-matrix c)
)

(defn get-camera [] @camera-matrix)

(defn world-light [l]
  (reset! world-light-vec (apply math/vec3f l))
)

(defn get-world-light [] @world-light-vec)

(defn push-matrix [] (swap! matrix-stack #(cons (first %) %)))
(defn pop-matrix [] (swap! matrix-stack rest))
(defn peek-matrix [] (first @matrix-stack))
(defn swap-matrix [f] (swap! matrix-stack #(cons (->> % first f) (rest %))))
(defn apply-matrix [m] (swap-matrix #(.mul m %)))
(defn reset-matrix-stack [] (reset! matrix-stack (list (math/mat-identity))))

(defn translate [x y z]
  (apply-matrix (math/translation x y z))
)

(defn rotate [x y z]
  (apply-matrix (math/rotation x y z))
)

(defn scale [x y z]
  (apply-matrix (math/scale x y z))
)

(defn load-shader [gl storage vert frag] (.load (. Shader loader storage gl) vert frag))

(defn load-graphics
  ([gl storage diffuse-shader skin-shader]
    (. Graphics collada gl diffuse-shader skin-shader storage)
  )
  ([gl storage diffuse-shader skin-shader geom-parser skin-parser anim-parser material-parser]
    (. Graphics collada gl diffuse-shader skin-shader storage geom-parser skin-parser anim-parser material-parser)
  )
)

(defn load-image-tex [gl tex]
  (let
    [
      bl [0 0 0]
      br [1 0 0]
      tl [0 1 0]
      tr [1 1 0]
      bl-uv [0 0]
      br-uv [1 0]
      tl-uv [0 1]
      tr-uv [1 1]
      to-float (partial map float)
      buf (comp vec to-float concat)
      geom (-> gl
        (.vao (. Buffer range 6))
        (.attribute 3 (. Buffer of (buf tl bl br br tr tl)))
        (.attribute 2 (. Buffer of (buf tl-uv bl-uv br-uv br-uv tr-uv tl-uv)))
        (.attribute 3 (. Buffer of (apply buf (repeat 6 [0 0 -1]))))
        (.drawing)
      )
      res { :gl gl :geom geom :tex tex :drawing (.apply tex geom) :disposed (atom false) }
    ]
    res
  )
)

(defn load-image [gl storage file]
  (load-image-tex gl
    (.texture gl (.open storage file))
  )
)

(defn load-image-img [gl img]
  (load-image-tex gl
    (.texture gl img)
  )
)

(defn unload-image [img]
  (->> img :tex .dispose)
  (->> img :geom .dispose)
  (-> img :disposed (reset! true))
  ()
)

(defn image [img shader x y w h]
  (let
    [
      player (.player shader)
      uniforms (.uniforms player)
      trans (.uniformMatrix uniforms "transform")
    ]
    (.draw
      (.play player
        (proxy [Drawing] []
          (draw []
            (.load trans (math/transform [x y 0] [0 0 0] [w h 1]))
            (cond
              (-> img :disposed deref true?) (throw (RuntimeException. "Texture cannot be used, it was disposed"))
              :else (-> img :drawing .draw)
            )
          )
        )
      )
    )
  )
)

(defn load-font [name] (text/font name))

(defn load-text [gl font]
  (let
    [
      img (text/text-image font)
      tex (load-image-img gl img)
    ]
    tex
  )
)

(defn unload-text [text] (unload-image text))

(defn text [img shader text x y w h]
  (let [
      geom (text/text-geom (img :gl) text)
      tex (img :tex)
      drawing (.apply tex geom)
    ]
    (image (assoc img :geom geom :drawing drawing) shader x y w h)
    (.dispose geom)
  )
)

(defn condition-func [func] (proxy [Condition] [] (match [t] (func t))))
(defn condition-equality [item] (. Condition equality item))

(defn load-materials [graphics file]
  (.materials graphics file)
)

(defn material-provider-colors [gl map]
  (proxy [MaterialProvider][]
    (material [name]
      (let [
          [cr cg cb ca] (get map name [1 0 1 1])
        ]
        (. Material textureAndColor (.blankTexture gl) (. Color floats cr cg cb ca))
      )
    )
  )
)

(defn material-provider-combine [a a-keys b]
  (let [
      a-keys (set a-keys)
      mat-fn (fn [name]
        (cond
          (contains? a-keys name) (.material a name)
          :else (.material b name)
        )
      )
      mat-fn (memoize mat-fn)
    ]
    (proxy [MaterialProvider] []
      (material [name] (mat-fn name))
    )
  )
)

(defn load-model [graphics file]
  {
    :model (.model graphics file)
    :materials (load-materials graphics file)
  }
)

(defn load-anim [graphics file] (.instance-always (.animation graphics file (condition-equality "JOINT"))))
(defn load-obj-anim [graphics file] (.instance-always (.animation graphics file (condition-equality "NODE"))))

(defn skeleton-func [func]
  (proxy [Skeleton] []
    (transform [b]
      (func b)
    )
  )
)

(defn anim-func [func]
  (proxy [AnimatedSkeleton] []
    (animate [t]
      (skeleton-func
        (fn [b] (func t b))
      )
    )
  )
)

(defn empty-skeleton [] (skeleton-func (constantly (math/mat-identity))))

(defn empty-anim [] (anim-func (constantly (empty-skeleton))))

(defn comp-anim [a b]
  (anim-func
    (fn [t k]
      (let [
          at (.transform (.animate a t) k)
          bt (.transform (.animate b t) k)
        ]
        (.mul at bt)
      )
    )
  )
)

(defn anim-parser-func [func bone-pred]
  (proxy [ColladaAnimationParser] []
    (read [node reader]
      (.read reader
        (proxy [Animator] []
          (animation [id name]
            (cond
              (bone-pred name)
              (proxy [Animation] []
                (animate [t]
                  (func t name)
                )
              )
              :else nil
            )
          )
        )
      )
    )
  )
)

(defn instance [source refresh-status] (.instance source refresh-status))
(defn instance-always [source] (.instance source (. RefreshStatus always)))

(defn load-anim-clj [storage bone-type anim-file model-file]
  (let [
      db ((comp read-string slurp) anim-file)
      bone-names ((comp set map) first (db :bones))
      resources (db :resources)
      len (db :length)
      len (if (zero? len) 1 len)
      flip (math/mat4f [-1 0 0 0    0 0 1 0   0 1 0 0    0 0 0 1])
      flip-mat (fn [m]
        (.mul (.mul flip (math/mat4f m)) flip)
      )
      anims (db :bones)
      anims (mapv
        (fn [[n v]]
          [n
            (let [
                [ks vs] (apply mapv vector v)
                vs (map #(mapv resources %) vs)
                mats (for [[c0 c1 c2 c3] vs]
                  (concat c0 [0] c1 [0] c2 [0] c3 [1])
                )
              ]
              (KeyFrameAnimation.
                (. Buffer of (mapv float ks))
                (. Buffer of (mapv flip-mat mats))
              )
            )
          ]
        )
        anims
      )
      anims (apply hash-map (apply concat anims))
      f (fn [t name]
        (.animate (anims name) t)
      )
      node (. ColladaNode fromFile (.open storage model-file))
      aparser (anim-parser-func f (partial contains? bone-names))
      sparser (ColladaBasicSkeletonParser. (condition-func bone-type))
      anim (. AnimatedSkeleton fromCollada node aparser sparser)
    ]
    {
      :anim (. AnimatedSkeleton cached (instance-always anim) bone-names 30 len)
      :length len
    }
  )
)

(defn load-animated-model [graphics file]
  {
    :model (.animatedModel graphics file)
    :materials (load-materials graphics file)
  }
)

(defn animated-model
  ([md anim obj-anim] (animated-model md anim obj-anim (md :materials)))
  ([md anim obj-anim mats]
    (.draw
      (.animate (md :model)
        (.mul
          (get-projection)
          (get-camera)
        )
        (get-world-light)
        (peek-matrix)
        anim
        obj-anim
        mats
      )
    )
  )
)

(defn animate [anim t] (.animate anim t))

(defn model
  ([md] (model md (md :materials)))
  ([md mats]
    (.draw
      (.transform (md :model)
        (.mul
          (get-projection)
          (get-camera)
        )
        (get-world-light)
        (peek-matrix)
        mats
      )
    )
  )
)
