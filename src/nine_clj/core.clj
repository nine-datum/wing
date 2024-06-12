(ns nine-clj.core
  (:gen-class)
  (:import
    [nine.lwjgl
      LWJGL_Window
      LWJGL_OpenGL
    ]
    [nine.function
      RefreshStatus
      UpdateRefreshStatus
    ]
    [nine.math
      Vector2f
      Vector3f
      Matrix4f
      FloatFunc
    ]
    [nine.opengl
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
  )
)

(defn floats-from-mat4f [m] ((comp vec map) #(.at m %) (range 16)))
(defn floats-from-vec2f [v] [(.x v) (.y v)])
(defn floats-from-vec3f [v] [(.x v) (.y v) (.z v)])

(defn vec2f [x y] (. Vector2f newXY x y))
(defn vec3f [x y z] (. Vector3f newXYZ x y z))
(defn mat4f [& fs] (. Matrix4f fromArray fs))

(defn radians [d]
  (. FloatFunc toRadians d)
)

(def window-width (atom 0))
(def window-height (atom 0))

(defn width [] @window-width)
(defn height [] @window-height)

(defn perspective [w h fov near far]
  (. Matrix4f perspective (/ w (float h)) fov near far)
)

(defn transform [pos rot scale]
  (. Matrix4f transform pos rot scale)
)

(defn translation [x y z]
  (. Matrix4f translation (vec3f x y z))
)

(defn orbital-camera [pos rot dist]
  (. Matrix4f orbitalCamera pos rot dist)
)

(def matrix-stack (atom (list (. Matrix4f identity))))
(def projection-matrix (atom (. Matrix4f identity)))

(defn projection [p]
  (reset! projection-matrix p)
)

(defn get-projection [] @projection-matrix)

(defn push-matrix [] (swap! matrix-stack #(cons (first %) %)))
(defn pop-matrix [] (swap! matrix-stack rest))
(defn peek-matrix [] (first @matrix-stack))
(defn apply-matrix [m] (swap! matrix-stack #(cons (-> % first (.mul m)) (rest %))))

(def storage (FileStorage.))

(def refresh-status (UpdateRefreshStatus.))

(defn load-shader [gl vert frag] (.load (. Shader loader storage gl) vert frag))

(defn load-graphics [gl diffuse-shader skin-shader]
  (. Graphics collada gl diffuse-shader skin-shader storage refresh-status)
)

(defn load-model [graphics file] (.model graphics file))

(defn model [m] (.draw (.transform m (get-projection) (vec3f 0 0 1) (peek-matrix))))

(def state (atom {}))

(defn windowLoop [id loop]
  (proxy [WindowLoopAction] []
    (call [w h]
      (.update refresh-status)
      (reset! matrix-stack (list (. Matrix4f identity)))
      (reset! window-width w)
      (reset! window-height h)
      (swap! state loop)
    )
  )
)

(defn windowStart [setup loop]
  (proxy [WindowStartAction] []
    (start [id]
      (.update refresh-status)
      (reset! state (setup))
      (windowLoop id loop)
    )
  )
)

(defn test-setup []
  (let
    [
      gl (LWJGL_OpenGL.)
      skin-shader (load-shader gl "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (load-shader gl "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (load-graphics gl diffuse-shader skin-shader)
      model (load-model graphics "res/models/Knight/Idle.dae")
    ]
    { :model model }
  )
)

(defn test-loop [state]
  (projection (perspective (width) (height) (radians 60) 0.01 100))
  (push-matrix)
  (apply-matrix (translation 0 -2 5))
  (model (state :model))
  (pop-matrix)
  state
)

(defn -main [& args]
  (.run (LWJGL_Window.) 600 400
    (windowStart test-setup test-loop)
  )
)
