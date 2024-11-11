(ns nine-clj.scenes.menu
  [:require
    [nine-clj.gui :as gui]
    [nine-clj.graph :as graph]
    [nine-clj.input :as input]
    [nine-clj.scenes.generic :as generic]
    [nine-clj.scenes.world :as world]
    [nine-clj.scenes.location :as location]
    [nine-clj.scripting :as scripting]
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
      ["Начать игру" (fn [dev res state] ((res :world-setup) dev res))]
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
      ["Выйти из города" (fn [dev res state] (-> state :resume-state :world-state-setup (apply [(state :resume-state)])))]
      ["Выйти в меню" (fn [dev res state] (menu-setup dev res))]
    ]
    location/location-render-loop
    resume-state
  )
)

(defn location-enter-menu-setup [dev res location-state-setup arena-state-setup exit-state-setup resume-state]
  (pause-menu-setup dev res []
    [
      ["Вы приблизились к городу" gui/aspect-fit-layout [1 1 1 1] [-0.5 0.4 1 0.2]]
    ]
    [
      ["Войти с миром" (fn [dev res state] (location-state-setup))]
      ["Напасть" (fn [dev res state] (arena-state-setup))]
      ["Уйти с миром" (fn [dev res state] (exit-state-setup state))]
    ]
    world/world-render-loop
    resume-state
  )
)

(declare loading-menu-loop)

(defn loading-menu-setup [dev res-atom setup]
  (let [
      { :keys [gl storage mouse width height] } dev
      gui-asset (gui/gui-asset (assoc dev :mouse (input/viewport-mouse mouse width height)))
      menu-image (graph/load-image gl storage "res/images/menu.png")
      [load-funcs res-func]  (-> "res/scripts/resources.clj" scripting/read-file (apply [dev]))
    ]
    (swap! res-atom #(assoc % :gui-asset gui-asset :menu-image menu-image))
    {
      :loop loading-menu-loop
      :images [
        [menu-image gui/aspect-fit-layout [-1.5 -1 3 2]]
      ]
      :texts [
        ["Загрузка..." gui/aspect-fit-layout [1 1 1 1] [-0.5 -0.1 1 0.2]]
      ]
      :buttons []
      :gui-asset gui-asset
      :res-atom res-atom
      :load-atom (atom {})
      :load-funcs load-funcs
      :res-func res-func
      :progress 0
      :max-progress (count load-funcs)
    }
  )
)

(defn loading-menu-loop [dev res state]
  (let [
      state (menu-loop dev res state)
      { :keys [gui-asset] } res
      { :keys [res-atom load-atom res-func load-funcs load-atom progress max-progress] } state
    ]
    (gui/text gui-asset gui/aspect-fit-layout (-> progress (/ max-progress) (* 100) int (str "%")) -0.5 -0.3 1 0.2 [1 1 1 1])
    (cond
      (empty? load-funcs) (menu-setup dev (reset! res-atom (res-func @load-atom)))
      :else (do
        (swap! load-atom (first load-funcs))
        (assoc state
          :progress (inc progress)
          :load-funcs (rest load-funcs)
        )
      )
    )
  )
)

(defmacro load-resources-let [root-name pairs expr]
  (let [
      pairs (partition 2 pairs)
      names (cons root-name (mapv first pairs))
      func-template (fn [[n e]]
        `(fn [lets#] (let [
            { :keys [~@names] } lets#
          ]
          (assoc lets# (keyword (name '~n)) ~e)
        ))
      )
      funcs (mapv func-template pairs)
      expr-func `(fn [lets#]
        (let [
            { :keys [~@names] } lets#
          ]
          ~expr
        )
      )
    ]
    (list funcs expr-func)
  )
)
