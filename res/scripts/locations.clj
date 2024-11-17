(require
  '[nine-clj.datum :as dat]
  '[nine-clj.math :as math]
  '[nine-clj.scenes.arena :as arena]
  '[nine-clj.graph :as graph]
)
(fn [dev world-markers all-presets]
  (let [
      marker (fn [loc marker-name]
        (.mul (loc :mat) (-> loc :name all-presets :markers (get marker-name)))
      )
      marker-pos (fn [m]
        (-> m (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
      )
      marker-look (fn [m]
        (-> m (.transformVector (math/vec3f 0 0 1)) math/floats-from-vec3f math/normalize)
      )
      marker-rot-y (fn [m]
        (as-> m v (marker-look v) (map v [0 2]) (apply math/clock v))
      )
      location (fn [& args]
        (let [
            h (apply hash-map args)
            { :keys [id name pos rot scale color side army recruits] } h
            mat (math/transform pos rot scale)
            h (assoc h :mat mat)
            entry (->> (get h :entry "entry"))
          ]
          {
            :id id
            :name name
            :preset (all-presets name)
            :models (->> name all-presets :models (mapv #(graph/replace-materials (dev :gl) % { "Flag-material" color })))
            :shapes (->> name all-presets :shapes (mapv #(assoc % :pos pos :rot rot)))
            :entry-pos (-> h (marker entry) marker-pos)
            :entry-look (-> h (marker entry) marker-look)
            :pos pos
            :rot rot
            :scale scale
            :spawn ((h :spawn) h)
            :army army
            :recruits recruits
            :color color
            :side side
          }
        )
      )
      guard-spawn (fn [info loc]
        (let [
            { :keys [color side] } loc
            ps (partition 2 info)
            posf #(->> % (marker loc) marker-pos)
            lookf #(->> % (marker loc) marker-look)
            ps (mapv (fn [[kind mark]] (vector kind color side (posf mark) (lookf mark))) ps)
          ]
          (fn [spawn-fn]
            (mapv #(apply spawn-fn %) ps)
          )
        )
      )
    ]
    {
      :castle-red (location
        :id :castle-red
        :name :castle
        :side :red
        :color [1 0 0 1]
        :pos (-> "castle_red" world-markers marker-pos)
        :rot [0 (-> "castle_red" world-markers marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial guard-spawn [
          :fighter "guard_0"
          :fighter "guard_1"
          :archer "guard_2"
          :archer "guard_3"
          :fighter "guard_4"
          :fighter "guard_5"
          :fighter "guard_6"
          :archer "guard_archer_1"
        ])
        :army (concat
          (repeat 5 :fighter)
          (repeat 10 :archer)
        )
        :recruits (concat
          (repeat 5 :fighter)
          (repeat 5 :archer)
        )
      )
      :castle-blue (location
        :id :castle-blue
        :name :castle
        :side :blue
        :color [0 0 1 1]
        :pos (-> "castle_blue" world-markers marker-pos)
        :rot [0 (-> "castle_blue" world-markers marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial guard-spawn [
          :archer "guard_0"
          :archer "guard_1"
          :archer "guard_2"
          :archer "guard_3"
          :archer "guard_4"
          :archer "guard_5"
          :archer "guard_6"
          :archer "guard_archer_1"
        ])
        :army (repeat 30 :archer)
        :recruits (repeat 10 :archer)
      )
      :castle-sand (location
        :id :castle-sand
        :name :castle
        :side :blue
        :color [219/255 154/255 89/255 1]
        :pos (-> "castle_sand" world-markers marker-pos)
        :rot [0 (-> "castle_sand" world-markers marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial guard-spawn [
          :ninja "guard_0"
          :ninja "guard_1"
          :ninja "guard_2"
          :ninja "guard_3"
          :ninja "guard_4"
          :ninja "guard_5"
          :ninja "guard_6"
          :ninja "guard_archer_1"
        ])
        :army (repeat 30 :ninja)
        :recruits (repeat 10 :ninja)
      )
    }
  )
)
