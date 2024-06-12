(ns nine-clj.core
  (:gen-class)
  (:import
    [nine.lwjgl
      LWJGL_Window
    ]
    [nine.opengl
      WindowStartAction
      WindowLoopAction
    ]
  )
)

(defn windowLoop [id] (proxy [WindowLoopAction] [] (call [w h] ())))

(defn windowStart [] (proxy [WindowStartAction] [] (start [id] (windowLoop id))))


(defn -main [& args]
  (println "Hello, World!")
  (.run (LWJGL_Window.) 600 400 (windowStart))
)
