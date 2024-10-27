(ns nine-clj.menu
  [:require
    [nine-clj.gui :as gui]
    [nine-clj.arena :as arena]
  ]
)

(declare menu-loop)

(defn menu-setup [dev res]
  {
    :loop menu-loop
    :gui-asset (res :gui-asset)
    :menu-image (res :menu-image)
    :buttons [
      ["Начать игру" (fn [dev res state] (arena/arena-setup dev res))]
      ["Настройки" (fn [dev res state] state)]
      ["Выход" (constantly nil)]
    ]
  }
)
(defn menu-loop [dev res state]
  (let [
      { :keys [gui-asset menu-image buttons] } state
      ** (gui/image gui-asset menu-image -1 -1 2 2 [1 1 1 1])
      bs (mapv (fn [[label func] i]
          (vector
            (gui/button gui-asset label -0.2 (-> i (+ 0.4) (* -0.2)) 0.4 0.15)
            func
          )
        )
        buttons
        (range)
      )
    ]
    (nth (->> bs (filter (comp true? first)) (map (comp #(% dev res state) last))) 0 state)
  )
)