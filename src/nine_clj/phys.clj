(ns nine-clj.phys
  (:import (com.bulletphysics.dynamics DiscreteDynamicsWorld RigidBody RigidBodyConstructionInfo)
           (com.bulletphysics.dynamics.constraintsolver SequentialImpulseConstraintSolver)
           (com.bulletphysics.collision.broadphase AxisSweep3)
           (com.bulletphysics.collision.dispatch CollisionDispatcher)
           (com.bulletphysics.collision.dispatch DefaultCollisionConfiguration)
           (com.bulletphysics.linearmath Transform DefaultMotionState)
           (com.bulletphysics.collision.shapes BoxShape)
           (javax.vecmath Vector3f Matrix4f Matrix3f)))

(defn create-physics-world []
  (let [collision-config (DefaultCollisionConfiguration.)
        dispatcher (CollisionDispatcher. collision-config)
        broadphase (AxisSweep3. (Vector3f. -10000 -10000 -10000) (Vector3f. 10000 10000 10000))
        solver (SequentialImpulseConstraintSolver.)
        dynamics-world (DiscreteDynamicsWorld. dispatcher broadphase solver collision-config)]
    (.setGravity dynamics-world (Vector3f. 0 -9.81 0))
    dynamics-world))

(defn add-rigid-body [dynamics-world shape mass position]
  (let [
        mat3 (Matrix3f.)
        mat3 (do (.setIdentity mat3) mat3)
        mat4 (Matrix4f. mat3 position (float 1))
        transform (Transform. mat4)
        motion-state (DefaultMotionState. transform)
        local-inertia (Vector3f. 0 0 0)]
    (.calculateLocalInertia shape mass local-inertia)
    (let [rbci (RigidBodyConstructionInfo. mass motion-state shape local-inertia)
          body (RigidBody. rbci)
      ]
      (.addRigidBody dynamics-world body)
      body)))

(def dynamics-world (create-physics-world))
(def box-shape (BoxShape. (Vector3f. 1 1 1)))
(def box-body (add-rigid-body dynamics-world box-shape 1.0 (Vector3f. 0 10 0)))

(defn update-physics [dynamics-world time-step]
  (.stepSimulation dynamics-world time-step))

(println "before" (.getCenterOfMassPosition box-body (Vector3f.)))
; В игровом цикле
(update-physics dynamics-world 1/60) ; Например, для 60 FPS
(println "after" (.getCenterOfMassPosition box-body (Vector3f.)))