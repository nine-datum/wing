(ns nine-clj.scenes.menu
  [:require
    [nine-clj.gui :as gui]
    [nine-clj.input :as input]
    [nine-clj.scenes.generic :as generic]
    [nine-clj.scenes.world :as world]
    [nine-clj.scenes.location :as location]
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
    :texts [
      [(str "nine-clj" (. System getProperty "nine-clj.version")) gui/aspect-fit-layout [0 1 0 1] [-0.5 -0.9 1 0.1]]
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
      { :keys [gui-asset images buttons texts] } state
      _ (doseq [[image layout [x y w h]] images] (gui/image gui-asset layout image x y w h [1 1 1 1]))
      _ (doseq [[label layout color [x y w h]] texts] (gui/text gui-asset layout label x y w h color))
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

(defn pause-menu-setup [dev res images texts buttons render-loop resume-state]
  {
    :loop pause-menu-loop
    :gui-asset (res :gui-asset)
    :buttons buttons
    :images images
    :texts texts
    :resume-state resume-state
    :render-loop render-loop
  }
)

(defn pause-menu-loop [dev res state]
  ((state :render-loop) dev res (state :resume-state))
  (cond
    (-> dev :keyboard input/escape-up) (state :resume-state)
    :else (menu-loop-base dev res state)
  )
)

(defn arena-pause-menu-setup [dev res resume-state]
  (pause-menu-setup dev res [] [] [
      ["Продолжить" (fn [dev res state] (state :resume-state))]
      ["Покинуть сражение" (fn [dev res state] (-> res :world-setup (apply [dev res])))]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    generic/generic-render-loop
    resume-state
  )
)

(defn world-pause-menu-setup [dev res resume-state]
  (pause-menu-setup dev res [] [] [
      ["Продолжить" (fn [dev res state] (state :resume-state))]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    world/world-render-loop
    resume-state
  )
)

(defn location-pause-menu-setup [dev res resume-state]
  (pause-menu-setup dev res [] [] [
      ["Продолжить" (fn [dev res state] (state :resume-state))]
      ["Выйти из города" (fn [dev res state] (-> state :resume-state :world-state))]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    location/location-render-loop
    resume-state
  )
)

(defn location-enter-menu-setup [dev res location-state exit-state resume-state]
  (pause-menu-setup dev res []
    [
      ["Войти в город?" gui/aspect-fit-layout [1 1 1 1] [-0.5 0.4 1 0.2]]
    ]
    [
      ["Да" (fn [dev res state] location-state)]
      ["Нет" (fn [dev res state] exit-state)]
    ]
    world/world-render-loop
    resume-state
  )
)
