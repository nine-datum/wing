(ns nine-clj.input
  [:require
    [nine-clj.math :as math]
  ]
  [:import
    [nine.lwjgl
      LWJGL_Keyboard
      LWJGL_Mouse
    ]
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

(defn wasd [kb]
  (mapv +
    (if (kb "w" :down) [0 1] [0 0])
    (if (kb "a" :down) [-1 0] [0 0])
    (if (kb "s" :down) [0 -1] [0 0])
    (if (kb "d" :down) [1 0] [0 0])
  )
)