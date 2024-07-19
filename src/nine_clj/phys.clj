(ns nine-clj.phys
  (:import
    (com.bulletphysics.dynamics
      DiscreteDynamicsWorld
      RigidBody
      RigidBodyConstructionInfo
    )
    (com.bulletphysics.dynamics.constraintsolver
      SequentialImpulseConstraintSolver
    )
    (com.bulletphysics.collision.broadphase
      AxisSweep3
    )
    (com.bulletphysics.collision.dispatch
      CollisionDispatcher
    )
    (com.bulletphysics.collision.dispatch
      DefaultCollisionConfiguration
    )
    (com.bulletphysics.linearmath
      Transform
      DefaultMotionState
    )
    (com.bulletphysics.collision.shapes
      BoxShape
      StaticPlaneShape
    )
    (javax.vecmath
      Vector3f
      Matrix4f
      Matrix3f
    )
  )
)

(defn create-physics-world []
  (let [
      collision-config (DefaultCollisionConfiguration.)
      dispatcher (CollisionDispatcher. collision-config)
      broadphase (AxisSweep3. (Vector3f. -10000 -10000 -10000) (Vector3f. 10000 10000 10000))
      solver (SequentialImpulseConstraintSolver.)
      dynamics-world (DiscreteDynamicsWorld. dispatcher broadphase solver collision-config)
    ]
    (.setGravity dynamics-world (Vector3f. 0 -9.81 0))
    dynamics-world
  )
)

(defn add-rigid-body [dynamics-world shape pos rot scale mass]
  (let [
      [px py pz] pos
      [rx ry rz] rot
      [sx sy sz] scale
      arr (make-array Float/TYPE 9)
      mat3 (Matrix3f.
        (do
          (aset arr 0 (float sx))
          (aset arr 4 (float sy))
          (aset arr 8 (float sz))
          arr
        )
      )
      mat3 (do
        (.rotX mat3 rx)
        (.rotY mat3 ry)
        (.rotZ mat3 rz)
        mat3
      )
      mat4 (Matrix4f. mat3 (Vector3f. px py pz) (float 1))
      transform (Transform. mat4)
      motion-state (DefaultMotionState. transform)
      local-inertion (Vector3f. 0 0 0)
    ]
    (.calculateLocalInertia shape mass local-inertion)
    (let [
        rbci (RigidBodyConstructionInfo. mass motion-state shape local-inertion)
        body (RigidBody. rbci)
      ]
      (.addRigidBody dynamics-world body)
      body
    )
  )
)

(def dynamics-world (create-physics-world))
(def box-shape (BoxShape. (Vector3f. 1 1 1)))

(defn box [pos rot scale mass]
  (add-rigid-body dynamics-world box-shape
    pos rot scale mass
  )
)

(defn plane[[nx ny nz] const]
  (add-rigid-body dynamics-world
    (StaticPlaneShape. (Vector3f. nx ny nz) const)
    [0 0 0] [0 0 0] [1 1 1] 0
  )
)

(defn set-velocity [body [vx vy vz]]
  (.setLinearVelocity body (Vector3f. vx vy vz))
  body
)

(defn set-angular-velocity [body [vx vy vz]]
  (.setAngularVelocity body (Vector3f. vx vy vz))
  body
)

(defn set-matrix [body mat]
  (let [
      m (Matrix4f.)
      cs (partition 4 (map float mat))
      m (doseq [[i [x y z w]] (map vector (range) cs)]
        (.setColumn m i x y z w)
      )
      t (Transform. m)
    ]
    (.setCenterOfMassTransform body t)
  )
  body
)

(defn set-rotation-enabled [body v]
  (.setAngularFactor body (if v 1 0))
  body
)

(defn set-position [body [x y z]]
  (let [
      t (.getCenterOfMassTransform body (Transform.))
      m (.getMatrix t (Matrix4f.))
      m (do (.setColumn 3 x y z 1) m)
      t (Transform. m)
    ]
    (.setCenterOfMassTransform body t)
  )
  body
)

(defn get-matrix [body]
  (let [
      t (.getCenterOfMassTransform body (Transform.))
      m (.getMatrix t (Matrix4f.))
      cs (mapv (partial make-array Float/TYPE) [4 4 4 4])
    ]
    (doseq [i (range 4)] (.getColumn m (int i) (cs i)))
    ((comp vec (partial apply concat)) cs)
  )
)

(defn get-position [body]
  (let [
      t (.getCenterOfMassTransform body (Transform.))
      m (.getMatrix t (Matrix4f.))
      fs (make-array Float/TYPE 4)
    ]
    (.getColumn m 3 fs)
    (vec (take 3 fs))
  )
)

(defn update [time-step]
  (.stepSimulation dynamics-world time-step)
)