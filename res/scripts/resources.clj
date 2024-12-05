(require
  '[wing.core :as core]
  '[wing.graph :as graph]
  '[wing.geom :as geom]
  '[wing.gui :as gui]
  '[wing.input :as input]
  '[wing.phys :as phys]
  '[wing.prof :as prof]
  '[wing.math :as math]
  '[wing.menu :as menu]
  '[wing.game :as game]
  '[wing.scripting :as scripting]
)
(use 'wing.core)

(fn [dev]
  (menu/load-resources-let [
      gl (dev :gl)
      storage (dev :storage)
      mouse (dev :mouse)
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      gui-asset (gui/gui-asset (assoc dev :mouse (input/viewport-mouse mouse width height)))
      menu-image (graph/load-image gl storage "res/images/menu.png")
      player (graph/load-animated-model graphics "res/player/flight.dae")
      anims (->>
        ["flight" "left" "right" "drop"]
        (map #(vector % (str "res/player/" % ".dae")))
        (map #(update % 1 (partial graph/load-anim graphics)))
        (into {})
      )
    ]
    {
      :skin-shader skin-shader
      :diffuse-shader diffuse-shader
      :graphics graphics
      :gui-asset gui-asset
      :menu-image menu-image
      :player player
      :anims anims
      :game-setup game/game-setup
    }
  )
)
