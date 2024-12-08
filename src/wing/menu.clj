(ns wing.menu
  [:require
    [wing.gui :as gui]
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.scripting :as scripting]
    [wing.mac :refer [-->]]
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
      [(str "wing" (. System getProperty "wing.version")) gui/aspect-fit-layout [1 1 0 1] [-0.5 -0.9 1 0.1]]
    ]
    :buttons [
      ["1 игрок" (fn [dev res state] ((res :game-setup) dev res 1))]
      ["2 игрока" (fn [dev res state] ((res :game-setup) dev res 2))]
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
      :texts []
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
      prog (/ progress max-progress)
      prog-col (math/lerpv [0 0 1 1] [0 1 0 1] prog)
      prog-back-col [1/2 1/2 1/2 1]
      load-title (-> load-funcs second
        (get :name "...")
        name
        (.replace "-" " ")
      )
      load-title (str "Загрузка : " load-title)
    ]
    (gui/text gui-asset gui/aspect-fit-layout load-title -0.5 -0.1 1 0.1 [1 1 1 1])
    (gui/status-bar gui-asset gui/aspect-fit-layout prog prog-back-col prog-col -0.5 -0.2 1 0.05)
    (cond
      (empty? load-funcs) (menu-setup dev (reset! res-atom ((res-func :func) @load-atom)))
      :else (do
        (swap! load-atom (-> load-funcs first :func))
        (assoc state
          :progress (inc progress)
          :load-funcs (rest load-funcs)
        )
      )
    )
  )
)

(defmacro load-resources-let [pairs expr]
  (let [
      pairs (partition 2 pairs)
      names (mapv first pairs)
      pair (fn [n f] { :name n :func f } )
      func-template (fn [[n e]] (pair (keyword n)
        `(fn [lets#] (let [
            { :keys [~@names] } lets#
          ]
          (assoc lets# (keyword (name '~n)) ~e)
        )))
      )
      funcs (mapv func-template pairs)
      expr-func (pair :final `(fn [lets#]
        (let [
            { :keys [~@names] } lets#
          ]
          ~expr
        )
      ))
    ]
    (list list funcs expr-func)
  )
)
