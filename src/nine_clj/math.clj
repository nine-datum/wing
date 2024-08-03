(ns nine-clj.math
  [:require
    [clojure.core.matrix :as mat]
  ]
  [:import
    [nine.math
      Vector2f
      Vector3f
      Matrix4f
      FloatFunc
    ]
  ]
)

(defn escape-nan [f] (if (Double/isNaN f) 0 f))

(defn mat-identity [] (. Matrix4f identity))

(defn vec-identity [] [
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1
  ]
)

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


(defn perspective [w h fov near far]
  (. Matrix4f perspective (/ w (float h)) fov near far)
)

(defn scale [x y z]
  (. Matrix4f scale (vec3f x y z))
)

(defn clock [x y]
  (let [
      len (mat/length [x y])
      [y x] (if (zero? len) [1 0] (mat/normalise [x y]))
    ]
    (* (Math/acos x) (if (> y 0) 1 -1))
  )
)

(defn transform [pos rot scale]
  (. Matrix4f transform (apply vec3f pos) (apply vec3f rot) (apply vec3f scale))
)

(defn translation [x y z]
  (. Matrix4f translation (vec3f x y z))
)

(defn rotation [x y z]
  (. Matrix4f rotation (vec3f x y z))
)

(defn get-vec-column-3 [mat n]
  (subvec (vec mat) (* n 4) (+ 3 (* n 4)))
)

(defn get-column-3 [mat n]
  (let [
      p (* n 4)
    ]
    [  
      (.at mat p)
      (.at mat (+ 1 p))
      (.at mat (+ 2 p))
    ]
  )
)

(defn orbital-camera [pos rot dist]
  (->
    (. Matrix4f translation (vec3f 0 0 dist))
    (.mul (. Matrix4f rotationX (- (.x rot))))
    (.mul (. Matrix4f rotationY (- (.y rot))))
    (.mul (. Matrix4f rotationZ (- (.z rot))))
    (.mul (. Matrix4f translation (.negative pos)))
  )
)

(defn first-person-camera [pos rot]
  (. Matrix4f firstPersonCamera (apply vec3f pos) (apply vec3f rot))
)

(defn lerp [a b t]
  (+ a (* t (- b a)))
)

(defn lerpv [a b t]
  (mapv lerp a b (repeat t))
)

(defn look-rot[dir]
  (let[
      [x y z] dir
      rx (Math/asin (+ y))
      ry (clock x z)
    ]
    [rx ry 0]
  )
)