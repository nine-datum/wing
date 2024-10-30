(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
)
(fn [phys-world presets]
  (let [
      make-char (fn [preset pos dir color side] (dat/load-char phys-world preset pos dir color side (core/get-time)))
      step (fn [n] (* (quot (inc n) 2) (Math/pow -1 (rem n 2))))
      step (comp (partial * 2) step)
      make-red (fn [preset pos look] (make-char preset pos look [1 0 0 1] :red))
      make-green (fn [preset pos look] (make-char preset pos look [0 1 0 1] :green))
      players (-> presets :ninja (make-green [-10 1 -10] [-1 0 0]) vector)
    ]
    players
  )
)