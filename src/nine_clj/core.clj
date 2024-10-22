(ns nine-clj.core
  (:gen-class)
  (:require
    [nine-clj.graph :as graph]
    [nine-clj.geom :as geom]
    [nine-clj.math :as math]
    [nine-clj.text :as text]
    [nine-clj.phys :as phys]
    [nine-clj.input :as input]
    [nine-clj.datum :as dat]
    [nine-clj.prof :as prof]
    [nine-clj.gui :as gui]
  )
  (:import
    [nine.lwjgl
      LWJGL_Window
    ]
    [nine.function
      UpdateRefreshStatus
    ]
    [nine.opengl
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
    [org.lwjgl.glfw
      GLFW
    ]
  )
)

(defn read-script [file]
  (with-open [r (clojure.java.io/reader file)]
    (let [
        p (java.io.PushbackReader. r)
        ex (loop [ex []]
          (let [e (read p false nil)]
            (cond (nil? e) ex :else (recur (conj ex e)))
          )
        )
        res (binding [*ns* (create-ns 'script)]
          (clojure.core/refer-clojure)
          (last (mapv eval ex))
        )
      ]
      res
    )
  )
)

(defn new-status [] (UpdateRefreshStatus.))
(defn update-status [status] (.update status))

(def proc-refresh-status (new-status))

(defn get-time [] (org.lwjgl.glfw.GLFW/glfwGetTime))

(def window-width (atom 0))
(def window-height (atom 0))

(defn width [] @window-width)
(defn height [] @window-height)

(def state (atom {}))

(defn windowLoop [id dev]
  (proxy [WindowLoopAction] []
    (call [w h]
      (cond (= (. GLFW GLFW_TRUE) (. GLFW glfwGetWindowAttrib id GLFW/GLFW_FOCUSED))
        (do
          (update-status proc-refresh-status)
          (graph/reset-matrix-stack)
          (reset! window-width w)
          (reset! window-height h)
          (swap! state (partial (@state :loop) dev))
        )
      )
      ((dev :mouse) :update)
      ((dev :keyboard) :update)
    )
  )
)

(defn window-start [setup]
  (proxy [WindowStartAction] []
    (start [id]
      (let [dev { :storage (FileStorage.) :gl (graph/new-gl) :keyboard (input/keyboard id) :mouse (input/mouse id proc-refresh-status) }]
        ((dev :mouse) :update)
        ((dev :keyboard) :update)
        (reset! state (setup dev))
        ;(org.lwjgl.glfw.GLFW/glfwSwapInterval 0) ; fps unlocker
        (windowLoop id dev)
      )
    )
  )
)

(defn window [w h setup params]
  (.run (LWJGL_Window.) w h
    (window-start setup)
  )
)

(defn -main [& args]
  (window 800 600 (read-script "res/scripts/arena_setup.clj") {})
)
