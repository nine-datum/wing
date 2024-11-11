(ns nine-clj.scenes.arena
  [:require
    [nine-clj.scenes.generic :as generic]
    [nine-clj.datum :as dat]
  ]
)

(declare arena-loop)

(defn arena-level [dev res spawn]
  (-> res :arena (assoc :spawn spawn))
)

(defn arena-setup [dev res level]
  (let
    [
      { :keys [arena-pause-menu-setup] } res
    ]
    (generic/generic-setup dev res arena-loop generic/generic-render-loop arena-pause-menu-setup level)
  )
)

(defn arena-loop [dev res state]
  (generic/generic-loop dev res state)
)

(defn arena-spawn [phys-world presets color-a color-b side-a side-b army-a army-b]
  (let [
      make-char (fn [preset pos dir color side] (dat/load-char phys-world preset pos dir color side :idle 0))
      step (fn [n] (* (quot (inc n) 2) (Math/pow -1 (rem n 2))))
      step (comp (partial * 2) step)
      make-b (fn [preset n] (make-char preset [(- 0 25 (* 2 (quot n 15))) 0 (step (mod n 10))] [1 0 0] color-b side-b))
      make-a (fn [preset n] (make-char preset [(+ 25 (* 2 (quot n 15))) 0 (step (mod n 10))] [-1 0 0] color-a side-a))
      players (concat
        (map make-a (map presets army-a) (range))
        (map make-b (map presets army-b) (range))
      )
    ]
    players
  )
)
