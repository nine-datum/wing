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
      player-wings-model (graph/load-animated-model graphics "res/player/player.dae")
      player-model (graph/load-animated-model graphics "res/player/player-no-wings.dae")
      parachute (graph/load-model graphics "res/player/parachute.dae")
      arrow (graph/load-model graphics "res/player/arrow.dae")
      anims (->>
        [
          "flight" "left" "right" "drop" "back"
          "idle" "walk" "run" "jump" "fall" "ball" "lost"
          "open_parachute" "parachuting"
        ]
        (map #(vector % (graph/load-anim-clj storage (partial = "JOINT")
              (str "res/anims/player/" % ".anim")
              "res/player/player.dae"
            )
          )
        )
        (into {})
      )
      levels (->>  {
          :city [ "Нью Йорк" "res/world/city.dae" ]
          :city-tex [ "Вашингтон" "res/world/city_tex.dae" ]
        }
        (mapv (fn [[name [label file]]] (vector name {
          :markers (geom/geom-markers-map storage file)
          :model (graph/load-model graphics file)
          :shapes (->>
            (geom/read-geom storage file)
            (mapv #(phys/geom-shape (% :vertex) (% :root)))
          )
          :label label
          :name name
        })))
        (into {})
      )
    ]
    {
      :skin-shader skin-shader
      :diffuse-shader diffuse-shader
      :graphics graphics
      :gui-asset gui-asset
      :menu-image menu-image
      :player-model player-model
      :player-wings-model player-wings-model
      :parachute parachute
      :arrow arrow
      :anims anims
      :game-setup game/game-setup
      :server-setup game/server-setup
      :client-setup game/client-setup
      :menu-setup menu/menu-setup
      :error-menu-setup menu/error-menu-setup
      :levels levels
      :net-port 8081
      :udp-port 8082
      :broadcast-port 8083
    }
  )
)
