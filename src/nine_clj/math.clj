(ns nine-clj.math
  [:import
    [nine.math
      Vector2f
      Vector3f
      Matrix4f
      FloatFunc
    ]
  ]
)

(defn mat-identity [] (. Matrix4f identity))

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
  (->
    (. Matrix4f translation (vec3f 0 0 dist))
    (.mul (. Matrix4f rotationX (- (.x rot))))
    (.mul (. Matrix4f rotationY (- (.y rot))))
    (.mul (. Matrix4f rotationZ (- (.z rot))))
    (.mul (. Matrix4f translation (.negative pos)))
  )
)