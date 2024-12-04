(ns wing.core
  (:gen-class)
  (:require
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.prof :as prof]
    [wing.menu :as menu]
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

(defn new-status [] (UpdateRefreshStatus.))
(defn update-status [status] (.update status))

(def proc-refresh-status (new-status))

(defn get-time [] (org.lwjgl.glfw.GLFW/glfwGetTime))

(def window-width (atom 0))
(def window-height (atom 0))

(defn width [] @window-width)
(defn height [] @window-height)

(def state (atom {}))
(def resources (atom {}))

(defn windowLoop [win id dev res-atom]
  (proxy [WindowLoopAction] []
    (call [w h]
      (cond
        (= (. GLFW GLFW_TRUE) (. GLFW glfwGetWindowAttrib id GLFW/GLFW_FOCUSED))
        (do
          (update-status proc-refresh-status)
          (graph/reset-matrix-stack)
          (graph/reset-camera)
          (graph/reset-light)
          (graph/reset-projection)
          (reset! window-width w)
          (reset! window-height h)
          (prof/reset)
          (prof/profile :main-loop (swap! state (partial (@state :loop) dev @res-atom)))
        )
      )
      ((dev :mouse) :update)
      (-> dev :keyboard input/keyboard-update)
      (cond (nil? @state) (.close win))
    )
  )
)

(defn window-start [win setup]
  (proxy [WindowStartAction] []
    (start [id]
      (let [
          dev {
            :storage (FileStorage.)
            :gl (graph/new-gl)
            :keyboard (input/keyboard id)
            :mouse (input/mouse id proc-refresh-status)
            :get-time get-time
            :width width
            :height height
          }
          res-atom resources
        ]
        ((dev :mouse) :update)
        (-> dev :keyboard input/keyboard-update)
        (reset! state (menu/loading-menu-setup dev res-atom setup))
        ;(org.lwjgl.glfw.GLFW/glfwSwapInterval 0) ; fps unlocker
        (windowLoop win id dev res-atom)
      )
    )
  )
)

(defn window [w h setup params]
  (let [win (LWJGL_Window.)]
    (.run win w h
      (window-start win setup)
    )
  )
)

(defn -main [& args]
  (window 800 600 (nth args 0 :menu-setup) {})
)
