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