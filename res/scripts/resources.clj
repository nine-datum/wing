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
  '[nine-clj.scripting :as scripting]
  '[nine-clj.scenes.arena :as arena]
  '[nine-clj.scenes.world :as world]
  '[nine-clj.scenes.menu :as menu]
  '[nine-clj.scenes.location :as location]
)
(use 'nine-clj.core)

(fn [dev]
  (let [
      { :keys [gl storage mouse] } dev
      skin-shader (graph/load-shader gl storage "res/shaders/diffuse_skin_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      diffuse-shader (graph/load-shader gl storage "res/shaders/diffuse_vertex.glsl" "res/shaders/diffuse_fragment.glsl")
      graphics (graph/load-graphics gl storage diffuse-shader skin-shader)
      arena-presets (dat/load-presets gl storage diffuse-shader skin-shader)
      world-presets (world/load-presets dev diffuse-shader skin-shader)
      all-presets (merge arena-presets world-presets)
      load-scene (fn [load-model-fn file]
        (hash-map
          :model (load-model-fn file)
          :shapes (->> file
            (geom/read-geom storage)
            (map #(map % [:vertex :root]))
            (mapv (partial apply phys/geom-shape))
          )
        )
      )
      script-load-func (fn [file]
        (fn [& args] (-> file scripting/read-file (apply args)))
      )
      arena (load-scene (partial graph/load-model graphics) "res/datum/scene/arena.dae")
      world (load-scene (partial world/load-world-model dev) "res/datum/scene/world/world.dae")
      world-locations (-> "res/scripts/locations.clj" script-load-func (apply [dev all-presets]))
      world-water (world/load-water-model dev "res/datum/scene/world/water.dae")
      arena-spawn (script-load-func "res/scripts/arena_spawn.clj")
      world-spawn (script-load-func "res/scripts/world_spawn.clj")
      gui-asset (gui/gui-asset (assoc dev :mouse (input/viewport-mouse mouse width height)))
      menu-image (graph/load-image gl storage "res/images/menu.png")
    ]
    {
      :skin-shader skin-shader
      :diffuse-shader diffuse-shader
      :graphics graphics
      :arena (assoc arena
        :presets arena-presets
        :spawn arena-spawn
        :update-state dat/update-game-state
        :update-phys phys/update-world
        :next-state dat/next-game-state
        :pos [0 0 0]
        :rot [0 0 0]
      )
      :world (assoc world
        :presets all-presets
        :spawn world-spawn
        :update-state (constantly ())
        :update-phys phys/update-world
        :next-state world/next-world-state
        :pos [0 0 0]
        :rot [0 0 0]
      )
      :world-locations world-locations
      :world-water world-water
      :gui-asset gui-asset
      :menu-image menu-image
      :arena-setup arena/arena-setup
      :world-setup world/world-setup
      :menu-setup menu/menu-setup
      :arena-pause-menu-setup menu/arena-pause-menu-setup
      :world-pause-menu-setup menu/world-pause-menu-setup
      :location-pause-menu-setup menu/location-pause-menu-setup
      :location-setup location/location-setup
    }
  )
)
