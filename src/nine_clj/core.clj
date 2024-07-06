(ns nine-clj.core
  (:gen-class)
  (:require
    [nine-clj.text :as text]
  )
  (:import
    [nine.lwjgl
      LWJGL_Window
      LWJGL_OpenGL
      LWJGL_Keyboard
      LWJGL_Mouse
    ]
    [nine.function
      RefreshStatus
      UpdateRefreshStatus
      FunctionDouble
    ]
    [nine.buffer
      Buffer
    ]
    [nine.math
      Vector2f
      Vector3f
      Matrix4f
      FloatFunc
      LocalTime
    ]
    [nine.opengl
      Drawing
      Shader
      WindowStartAction
      WindowLoopAction
    ]
    [nine.io
      FileStorage
    ]
    [nine.game
      Graphics
    ]
    [nine.geometry.procedural
      Geometry
    ]
    [nine.geometry
      Skeleton
      AnimatedSkeleton
      Animation
    ]
    [nine.geometry.collada
      ColladaBasicAnimationParser
      ColladaBasicSkeletonParser
      ColladaBasicGeometryParser
      ColladaAnimationReader
      ColladaAnimationParser
      ColladaGeometryParser
      ColladaNode
      BuffersReader
      BufferMapping
    ]
    [java.util
      Arrays
    ]
  )
)

(defn new-gl [] (LWJGL_OpenGL.))

(defn floats-from-mat4f [m] ((comp vec map) #(.at m %) (range 16)))
(defn floats-from-vec2f [v] [(.x v) (.y v)])
(defn floats-from-vec3f [v] [(.x v) (.y v) (.z v)])

(defn vec2f [x y] (. Vector2f newXY x y))
(defn vec3f [x y z] (. Vector3f newXYZ x y z))
(defn mat4f [fs]
  (let [
      ar (make-array Float/TYPE 16)
      v (vec fs)
    ]
    (doseq [i (range 16)] (aset ar i (float (v i))))
    (. Matrix4f fromArray ar)
  )
)

(defn radians [d]
  (. FloatFunc toRadians d)
)

(def time-obj (LocalTime.))
(defn get-time [] (.value time-obj))

(def window-width (atom 0))
(def window-height (atom 0))

(defn width [] @window-width)
(defn height [] @window-height)

(defn mat-identity [] (. Matrix4f identity))

(defn perspective [w h fov near far]
  (. Matrix4f perspective (/ w (float h)) fov near far)
)

(defn scale [x y z]
  (. Matrix4f scale (vec3f x y z))
)

(defn transform [pos rot scale]
  (. Matrix4f transform pos rot scale)
)

(defn translation [x y z]
  (. Matrix4f translation (vec3f x y z))
)

(defn rotation [x y z]
  (. Matrix4f rotation (vec3f x y z))
)

(defn orbital-camera [pos rot dist]
  (. Matrix4f orbitalCamera pos rot dist)
)

(def matrix-stack (atom (list (. Matrix4f identity))))
(def projection-matrix (atom (. Matrix4f identity)))
(def camera-matrix (atom (. Matrix4f identity)))
(def world-light-vec (atom (. Vector3f newXYZ 0 0 1)))

(defn projection [p]
  (reset! projection-matrix p)
)

(defn get-projection [] @projection-matrix)

(defn camera [c]
  (reset! camera-matrix c)
)

(defn get-camera [] @camera-matrix)

(defn world-light [l]
  (reset! world-light-vec l)
)

(defn get-world-light [] @world-light-vec)

(defn push-matrix [] (swap! matrix-stack #(cons (first %) %)))
(defn pop-matrix [] (swap! matrix-stack rest))
(defn peek-matrix [] (first @matrix-stack))
(defn swap-matrix [f] (swap! matrix-stack #(cons (->> % first f) (rest %))))
(defn apply-matrix [m] (swap-matrix #(.mul m %)))

(def storage (FileStorage.))

(def refresh-status (UpdateRefreshStatus.))

(defn load-shader [gl vert frag] (.load (. Shader loader storage gl) vert frag))

(defn load-graphics
  ([gl diffuse-shader skin-shader]
    (. Graphics collada gl diffuse-shader skin-shader storage refresh-status)
  )
  ([gl diffuse-shader skin-shader geom-parser skin-parser anim-parser material-parser]
    (. Graphics collada gl diffuse-shader skin-shader storage refresh-status geom-parser skin-parser anim-parser material-parser)
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
      res { :geom geom :tex tex :drawing (.apply tex geom) :disposed (atom false) }
    ]
    res
  )
)

(defn load-image [gl file]
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
            (.load trans (transform (vec3f x y 0) (vec3f 0 0 0) (vec3f w h 1)))
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

(defn load-text [gl text font]
  (let
    [
      img (text/text-image text font)
      tex (load-image-img gl img)
    ]
    tex
  )
)

(defn unload-text [text] (unload-image text))

(defn text [tx shader x y w h]
  (image tx shader x y w h)
)

(defn live-text [gl shader]
  (fn [str font x y w h]
    (let [t (load-text gl str font)]
      (text t shader x y w h)
      (unload-text t)
    )
  )
)

(defn load-model [graphics file] (.model graphics file))

(defn load-anim [graphics file] (.animation graphics file "JOINT"))


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

(defn empty-skeleton [] (skeleton-func (constantly (mat-identity))))

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

(defn anim-process-func [func]
  (proxy [FunctionDouble] []
    (call [name, anim]
      (proxy [Animation] []
        (animate [t]
          (.mul
            (.animate anim t)
            (func t name)
          )
        )
      )
    )
  )
)

(defn anim-parser-func [func bones]
  (proxy [ColladaAnimationParser] []
    (read [node reader]
      (doseq [bone bones]
        (->>
          (proxy [Animation] []
            (animate [t] (func t bone))
          )
          (.read reader (str "Armature_" bone))
          (.read reader bone)
        )
      )
    )
  )
)

(defn geom-offset-parser [offset]
  (proxy [ColladaGeometryParser] []
    (read [node reader]
      (.read (ColladaBasicGeometryParser.) node
        (proxy [BuffersReader] []
          (read [source mat floats ints]
            (.read reader source mat
              (proxy [BufferMapping] []
                (map [semantic]
                  (let [sr (.map floats semantic)]
                    (case semantic "VERTEX"
                      (proxy [Buffer] []
                        (length [] (.length sr))
                        (at [i]
                          (+ (.at sr i) (offset (mod i 3)))
                        )
                      )
                      sr
                    )
                  )
                )
              )
              ints
            )
          )
        )
      )
    )
  )
)

(defn load-offset-graphics [gl offset]
  ()
)

(defn load-anim-clj [anim-file model-file]
  (let [
      db ((comp read-string slurp) anim-file)
      bone-names ((comp vec map) first (db :bones))
      f
      (fn [t b]
        (let [
            t (mod t (db :length))
            ls ((db :bones) b)
            ft (drop-while #(> t (first %)) ls)
            ft (vec ft)
            [k m] (get ft 0 (first ls))
            mat (mat4f m)
            flip (mat4f [-1 0 0 0    0 0 1 0   0 1 0 0    0 0 0 1])
            mat (.mul (.mul flip mat) flip)
          ]
          mat
        )
      )
      node (. ColladaNode fromFile (.open storage model-file))
      aparser (anim-parser-func f bone-names)
      sparser (ColladaBasicSkeletonParser. "JOINT")
      anim (. AnimatedSkeleton fromCollada node aparser sparser refresh-status)
    ]
    anim
  )
)

(defn load-obj-anim [graphics file] (.animation graphics file "NODE"))

(defn load-animated-model [graphics file] (.animatedModel graphics file))

(defn animated-model [m anim obj-anim]
  (.draw
    (.animate m
      (.mul
        (get-projection)
        (get-camera)
      )
      (get-world-light)
      (peek-matrix)
      anim
      obj-anim
    )
  )
)

(defn animate [anim t] (.animate anim t))

(defn model [m]
  (.draw
    (.transform m
      (.mul
        (get-projection)
        (get-camera)
      )
      (get-world-light)
      (peek-matrix)
    )
  )
)

(defn mouse [wid]
  (LWJGL_Mouse. wid refresh-status)
)

(defn keyboard [wid]
  (let [
      k (LWJGL_Keyboard. wid)
      m { :down (memfn isDown) :up (memfn isUp) }
      f (memfn keyOf symbol)
      f (partial f k)
    ]
    (fn 
      ([s] (case s :update (.update k) :else ()))
      ([c s]
        ((comp (partial apply (m s)) vector f first) c)
      )
    )
  )
)

(def state (atom {}))

(defn windowLoop [id dev loop]
  (proxy [WindowLoopAction] []
    (call [w h]
      (.update refresh-status)
      (reduce (comp apply) [dev [:keyboard] [:update]])
      (reset! matrix-stack (list (. Matrix4f identity)))
      (reset! window-width w)
      (reset! window-height h)
      (swap! state (partial loop dev))
    )
  )
)

(defn windowStart [setup loop]
  (proxy [WindowStartAction] []
    (start [id]
      (let [dev { :keyboard (keyboard id) }]
        (.update refresh-status)
        (reset! state (setup dev))
        (windowLoop id dev loop)
      )
    )
  )
)

(defn test-setup [dev]
  (let
    [
      gl (new-gl)
      skin-shader (load-shader gl "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (load-shader gl "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (load-graphics gl diffuse-shader skin-shader)
      model (load-animated-model graphics "res/datum/ninja.dae")
      anim (load-anim graphics "res/datum/mage.dae")
      obj-anim (load-obj-anim graphics "res/datum/ninja.dae")
      clj-anim (load-anim-clj "res/datum/anims/ninja/walk.clj" "res/datum/ninja.dae")
      scene (load-model graphics "res/models/Scenes/Mountains.dae")
      image (load-image gl "res/images/example.png")
      image-shader (load-shader gl "res/shaders/image_vertex.glsl" "res/shaders/image_fragment.glsl")
      font (text/default-font 12)
      text-shader image-shader
      textfn (live-text gl text-shader)
    ]
    {
      :font font
      :textfn textfn
      :model model
      :anim clj-anim
      :obj-anim clj-anim
      :scene scene
      :image image
      :image-shader image-shader
    }
  )
)

(defn test-loop [dev state]
  (projection (perspective (width) (height) (radians 60) 0.01 100))
  (camera (orbital-camera (vec3f 0 2 0) (vec3f 0 0 0) 5))
  (model (state :scene))
  
  (push-matrix)
  (apply-matrix (translation 0 0 0))
  (apply-matrix (rotation 0 (get-time) 0))
  (animated-model (state :model) (animate (state :anim) (get-time)) (animate (state :obj-anim) (get-time)))
  (pop-matrix)
  
  (image (state :image) (state :image-shader) -1 -0.5 0.5 0.5)
  (when ((dev :keyboard) "c" :down)
    (doseq [i (range 0 40)]
      ((state :textfn) "Hello, text!" (state :font) -0.5 (- 1 (* i 0.05)) 0.5 0.05)
    )
  )
  state
)

(defn window [w h setup loop params]
  (.run (LWJGL_Window.) w h
    (windowStart setup loop)
  )
)

(defn -main [& args]
  (window 800 600 test-setup test-loop {})
)
