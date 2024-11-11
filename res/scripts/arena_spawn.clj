(require
  '[nine-clj.datum :as dat]
)
(fn [phys-world presets]
  (let [
      make-char (fn [preset pos dir color side] (dat/load-char phys-world preset pos dir color side :idle 0))
      step (fn [n] (* (quot (inc n) 2) (Math/pow -1 (rem n 2))))
      step (comp (partial * 2) step)
      make-red (fn [preset n] (make-char preset [(- 0 25 (* 2 (quot n 15))) 0 (step (mod n 10))] [1 0 0] [1 0 0 1] :red))
      make-green (fn [preset n] (make-char preset [(+ 25 (* 2 (quot n 15))) 0 (step (mod n 10))] [-1 0 0] [0 1 0 1] :green))
      players (concat
        (map make-green (-> presets vals cycle) (range 120))
        (map make-red (-> presets vals cycle) (range 120))
      )
    ]
    players
  )
)
