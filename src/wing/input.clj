(ns wing.input
  [:require
    [wing.math :as math]
  ]
  [:import
    [nine.lwjgl
      LWJGL_Keyboard
      LWJGL_Mouse
    ]
    [nine.input Keyboard]
    [nine.math
      Vector2f
    ]
  ]
)

(defn mouse [wid status]
  (let [
      m (LWJGL_Mouse. wid status)
    ]
    (fn [key]
      (case key
        :delta (math/floats-from-vec2f(.delta m))
        :pos (math/floats-from-vec2f(.position m))
        :update ()
        :left-down (-> m .left .isDown)
        :right-down (-> m .right .isDown)
        :middle-down (-> m .middle .isDown)
        :left-up (-> m .left .isUp)
        :right-up (-> m .right .isUp)
        :middle-up (-> m .middle .isUp)
      )
    )
  )
)

(defn viewport-mouse [mouse width-fun height-fun]
  (fn [key]
    (let [
        w (width-fun)
        h (height-fun)
        [mx my] (mouse :pos)
        my (- h my)
      ]
      (case key
        :pos (->>
          [w h]
          (mapv / [mx my])
          (mapv + [-0.5 -0.5])
          (mapv * [2 2])
        )
        :delta (mapv / (mouse :delta) [(width-fun) (height-fun)])
        (mouse key)
      )
    )
  )
)

(defn keyboard-update [dev](.update dev))

(defn keyboard [wid]
  (LWJGL_Keyboard. wid)
)

(defn key-down [^Keyboard keyboard key] (-> keyboard (.keyOf key) .isDown))
(defn key-up [^Keyboard keyboard key] (-> keyboard (.keyOf key) .isUp))

(defn escape-down [^Keyboard keyboard] (-> keyboard .escape .isDown))
(defn escape-up [^Keyboard keyboard] (-> keyboard .escape .isUp))
(defn backspace-down [^Keyboard keyboard] (-> keyboard .backspace .isDown))
(defn backspace-up [^Keyboard keyboard] (-> keyboard .backspace .isUp))
(defn shift-down [^Keyboard keyboard] (-> keyboard .leftShift .isDown))
(defn shift-up [^Keyboard keyboard] (-> keyboard .leftShift .isUp))
(defn space-down [^Keyboard keyboard] (-> keyboard .space .isDown))
(defn space-up [^Keyboard keyboard] (-> keyboard .space .isUp))
(defn enter-down [^Keyboard keyboard] (-> keyboard (.keyOfIndex org.lwjgl.glfw.GLFW/GLFW_KEY_ENTER) .isDown))
(defn enter-up [^Keyboard keyboard] (-> keyboard (.keyOfIndex org.lwjgl.glfw.GLFW/GLFW_KEY_ENTER) .isUp))
(defn tab-down [^Keyboard keyboard] (-> keyboard .tab .isDown))
(defn tab-up [^Keyboard keyboard] (-> keyboard .tab .isUp))

(defn wasd [kb]
  (mapv +
    (if (key-down kb \w) [0 1] [0 0])
    (if (key-down kb \a) [-1 0] [0 0])
    (if (key-down kb \s) [0 -1] [0 0])
    (if (key-down kb \d) [1 0] [0 0])
  )
)

(defn arrows [kb]
  (mapv +
    (if (-> kb .arrowUp .isDown) [0 1] [0 0])
    (if (-> kb .arrowLeft .isDown) [-1 0] [0 0])
    (if (-> kb .arrowDown .isDown) [0 -1] [0 0])
    (if (-> kb .arrowRight .isDown) [1 0] [0 0])
  )
)
