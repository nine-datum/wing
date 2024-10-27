(require
  '[nine-clj.datum :as dat]
  '[nine-clj.core :as core]
  '[nine-clj.graph :as graph]
  '[nine-clj.geom :as geom]
  '[nine-clj.gui :as gui]
  '[nine-clj.input :as input]
  '[nine-clj.phys :as phys]
  '[nine-clj.prof :as prof]
  '[nine-clj.math :as math]
  '[nine-clj.arena :as arena]
  '[nine-clj.menu :as menu]
)
(use 'nine-clj.core)
(fn [dev]
  (let [
      { :keys [gl storage mouse] } dev
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      char-presets (dat/load-presets gl storage diffuse-shader skin-shader)
      arena (graph/load-model graphics "res/datum/scene/arena.dae")
      arena-geom (geom/read-geom storage "res/datum/scene/arena.dae")
      arena-geom (mapv :vertex arena-geom)
      arena-shape (mapv phys/geom-shape arena-geom)
      gui-asset (gui/gui-asset (assoc dev :mouse (input/viewport-mouse mouse width height)))
      menu-image (graph/load-image gl storage "res/images/menu.png")
    ]
    {
      :skin-shader skin-shader
      :diffuse-shader diffuse-shader
      :graphics graphics
      :char-presets char-presets
      :arena arena
      :arena-shape arena-shape
      :gui-asset gui-asset
      :menu-image menu-image
      :arena-setup arena/arena-setup
      :menu-setup menu/menu-setup
    }
  )
)