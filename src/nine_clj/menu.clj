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
  }
)
(defn menu-loop [dev state]
  (let [
      { :keys [gui-asset menu-image] } state
    ]
    (gui/image gui-asset menu-image -1 -1 2 2 [1 1 1 1])
    (mapv (fn [t i] (gui/button gui-asset t -0.2 (-> i (+ 0.4) (* -0.2)) 0.4 0.15))
      ["Начать игру" "Настройки" "Выйти"]
      (range)
    )
    state
  )
)