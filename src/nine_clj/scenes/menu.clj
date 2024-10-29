(ns nine-clj.scenes.menu
  [:require
    [nine-clj.gui :as gui]
    [nine-clj.input :as input]
  ]
)

(declare menu-loop)
(declare pause-menu-loop)

(defn menu-setup [dev res]
  {
    :loop menu-loop
    :gui-asset (res :gui-asset)
    :images [
      [(res :menu-image) gui/aspect-fit-layout [-1.5 -1 3 2]]
    ]
    :buttons [
      ["Начать игру" (fn [dev res state] ((res :arena-setup) dev res))]
      ["Настройки" (fn [dev res state] state)]
      ["Выход" (constantly nil)]
    ]
  }
)
(defn menu-loop-base [dev res state]
  (let [
      { :keys [gui-asset images buttons] } state
      _ (mapv
        (fn [[image layout [x y w h]]] (gui/image gui-asset layout image x y w h [1 1 1 1]))
        images
      )
      bs (mapv (fn [[label func] i]
          (vector
            (gui/button gui-asset gui/aspect-fit-layout label -0.5 (-> i (+ 0.4) (* -0.2)) 1 0.1)
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
(defn menu-loop [dev res state]
  (doto (dev :gl)
    (.clearDepth)
    (.clearColor 0 0 0 0)
  )
  (menu-loop-base dev res state)
)

(defn pause-menu-setup [dev res resume-state]
  {
    :loop pause-menu-loop
    :gui-asset (res :gui-asset)
    :buttons [
      ["Продолжить" (fn [dev res state] (state :resume-state))]
      ["Покинуть сражение" (fn [dev res state] state)]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    :images []
    :resume-state resume-state
  }
)

(defn pause-menu-loop [dev res state]
  (-> res :arena-render-loop (apply [dev res (state :resume-state)]))
  (cond
    (-> dev :keyboard input/escape-up) (state :resume-state)
    :else (menu-loop-base dev res state)
  )
)