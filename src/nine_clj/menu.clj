(ns nine-clj.menu
  [:require
    [nine-clj.gui :as gui]
  ]
)

(declare menu-loop)

(defn menu-setup [dev res]
  {
    :loop menu-loop
    :gui-asset (res :gui-asset)
    :menu-image (res :menu-image)
    :buttons [
      ["Начать игру" (fn [dev res state] ((res :arena-setup) dev res))]
      ["Настройки" (fn [dev res state] state)]
      ["Выход" (constantly nil)]
    ]
  }
)
(defn menu-loop [dev res state]
  (doto (dev :gl)
    (.clearDepth)
    (.clearColor 0 0 0 0)
  )
  (let [
      { :keys [gui-asset menu-image buttons] } state
      ** (gui/image gui-asset gui/aspect-fit-layout menu-image -1.5 -1 3 2 [1 1 1 1])
      bs (mapv (fn [[label func] i]
          (vector
            (gui/button gui-asset gui/aspect-fit-layout label -0.4 (-> i (+ 0.4) (* -0.2)) 0.8 0.15)
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