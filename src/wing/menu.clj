(ns wing.menu
  [:require
    [wing.gui :as gui]
    [wing.graph :as graph]
    [wing.math :as math]
    [wing.input :as input]
    [wing.client :as client]
    [wing.scripting :as scripting]
    [wing.mac :refer [--> funcall]]
    [wing.io :refer [save-profile load-profile]]
  ]
)

(declare menu-loop)
(declare menu-loop-base)
(declare pause-menu-loop)
(declare play-menu-setup)
(declare profile-menu-setup)

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
      ["Играть" (fn [dev res state] (play-menu-setup dev res))]
      ["Профиль" (fn [dev res state] (profile-menu-setup dev res))]
      ["Выход" (constantly nil)]
    ]
  }
)

(defn profile-menu-loop [dev res state]
  (doto (dev :gl)
    (.clearDepth)
    (.clearColor 1/2 1/2 1/2 1)
  )
  (let [
      { :keys [player-model anims] } res
      asset (state :gui-asset)
      layout gui/aspect-fit-layout
      anim (-> anims (get "walk") :anim (graph/animate (-> dev :get-time funcall)))
      model (graph/replace-materials (dev :gl) player-model { "ColorA-material" (state :color)})
      w (-> dev :width funcall)
      h (-> dev :height funcall)
      colors (->> (range 64)
        (map #(vector (quot (rem % 16) 4) (rem % 4) (quot % 16)))
        (map #(mapv / % (repeat 4)))
        (map #(conj % 1))
      )
      cs (mapv
        (fn [c i]
          [(gui/blank-button asset layout
            (-> i (rem 8) (* 0.1))
            (-> i (quot 8) (* 0.1) (- 0.5))
            0.1 0.1 c c c) c]
        )
        colors
        (range)
      )
      _ (gui/text asset layout "Выберите цвет" -0.5 0.5 1 0.1 [1 1 0 1])
      color-selected (nth (->> cs (filter first) first) 1 (state :color))
      ok? (gui/button asset layout "Принять" -0.5 -0.7 1 0.1)
      cancel? (gui/button asset layout "Отмена" -0.5 -0.9 1 0.1)
    ]
    (graph/projection (math/perspective w h (* Math/PI 1/3) 0.3 1000))
    (graph/camera (math/first-person-camera [0 1 -3] [0 0 0]))
    (graph/push-matrix)
    (graph/translate -1 0 0)
    (graph/rotate 0 Math/PI 0)
    (graph/animated-model model anim nil)
    (graph/pop-matrix)
    (cond
      ok? (do (save-profile (select-keys state [ :color ])) (menu-setup dev res))
      cancel? (menu-setup dev res)
      :else (assoc state :color color-selected)
    )
  )
)

(defn profile-menu-setup [dev res]
  (let [
      color (get (load-profile) :color [1 0 0 1])
    ]
    {
      :gui-asset (res :gui-asset)
      :loop profile-menu-loop
      :color color
      :buttons [
        ["Принять" (fn [dev res state] (do
          (save-profile (select-keys state [ :color ]))
          (menu-setup dev res)
        ))]
        ["Отмена" (fn [dev res state] (menu-setup dev res))]
      ]
    }
  )
)

(defn connect-menu-loop [dev res state]
  (let [
      { :keys [servers gui-asset color] } state
      layout gui/aspect-fit-layout
      _ (menu-loop dev res state)
      bs (mapv
        (fn [serv i]
          (assoc serv :press? (gui/button gui-asset layout
            (str (serv :name) " " (serv :addr))
            -0.5 (* 1/10 (inc i) -1) 1 0.1
          ))
        )
        @servers
        (range)
      )
      calcel (gui/button gui-asset layout "Назад" -0.5 -0.5 1 0.1)
      serv (->> bs (filter :press?) first)
    ]
    (cond
      serv ((res :client-setup) dev res (serv :level) (serv :addr) color "player")
      calcel (play-menu-setup dev res)
      :else state
    )
  )
)

(defn connect-menu-setup [dev res color]
  (let [
      servers (atom ())
    ]
    (client/discover-servers (res :broadcast-port)
      (fn [addr data] (swap! servers (partial cons (assoc (read-string data) :addr addr))))
    )
    {
      :loop connect-menu-loop
      :color color
      :servers servers
      :gui-asset (res :gui-asset)
      :images [
        [(res :menu-image) gui/aspect-fit-layout [-1.5 -1 3 2]]
      ]
    }
  )
)

(defn level-menu-setup [dev res setup]
  {
    :loop menu-loop
    :gui-asset (res :gui-asset)
    :images [
      [(res :menu-image) gui/aspect-fit-layout [-1.5 -1 3 2]]
    ]
    :buttons (conj (->> res :levels vals
      (mapv #(vector (-> % :label str) (fn [dev res state] (-> % :name setup))))
    ) ["Назад" (fn [dev res state] (play-menu-setup dev res))])
  }
)

(defn play-menu-setup [dev res]
  (let [
      color (-> load-profile funcall (get :color [1 0 0 1]))
    ]
    {
      :loop menu-loop
      :gui-asset (res :gui-asset)
      :images [
        [(res :menu-image) gui/aspect-fit-layout [-1.5 -1 3 2]]
      ]
      :buttons [
        ["1 игрок" (fn [dev res state]
            (level-menu-setup dev res #((res :game-setup) dev res % [color]))
          )
        ]
        ["2 игрока" (fn [dev res state]
            (level-menu-setup dev res #((res :game-setup) dev res % (vector color [0 0 1 1])))
          )
        ]
        ["Создать сервер" (fn [dev res state]
            (level-menu-setup dev res #((res :server-setup) dev res % color "player"))
          )
        ]
        ["Подключиться" (fn [dev res state] (connect-menu-setup dev res color))]
        ["Назад" (fn [dev res state] (menu-setup dev res))]
      ]
    }
  )
)

(defn error-menu-setup [dev res message]
  {
    :loop menu-loop
    :gui-asset (res :gui-asset)
    :images [
      [(res :menu-image) gui/aspect-fit-layout [-1.5 -1 3 2]]
    ]
    :texts [
      [message gui/aspect-fit-layout [1 1 0 1] [-0.5 0.1 1 0.1]]
    ]
    :buttons [
      ["В меню" (fn [dev res state] (menu-setup dev res))]
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
