(require
  '[nine-clj.datum :as dat]
  '[nine-clj.math :as math]
  '[nine-clj.scenes.arena :as arena]
  '[nine-clj.graph :as graph]
  '[nine-clj.nav :as nav]
)
(fn [dev world-markers all-presets]
  (let [
      location (fn [& args]
        (let [
            h (apply hash-map args)
            { :keys [id name pos rot scale color side army recruits] } h
            mat (math/transform pos rot scale)
            preset (all-presets name)
            h (assoc h :mat mat :preset preset)
            entry (->> (get h :entry "entry"))
          ]
          {
            :id id
            :name name
            :preset preset
            :models (->> preset :models (mapv #(graph/replace-materials (dev :gl) % { "Flag-material" color })))
            :shapes (->> preset :shapes (mapv #(assoc % :pos pos :rot rot)))
            :entry-pos (-> h (nav/marker entry) nav/marker-pos)
            :entry-look (-> h (nav/marker entry) nav/marker-look)
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
            posf #(->> % (nav/marker loc) nav/marker-pos)
            lookf #(->> % (nav/marker loc) nav/marker-look)
            ps (mapv (fn [[kind mark]] (vector kind color side
              (posf mark)
              (lookf mark)
              dat/passive-ai-next
              dat/passive-ai-in)) ps)
          ]
          (fn [spawn-fn]
            (mapv #(apply spawn-fn %) ps)
          )
        )
      )
      crowd-spawn (fn [loc]
        (let [
            { :keys [color side recruits] } loc
            pts (nav/location-nav loc)
          ]
          (fn [spawn-fn]
            (mapv #(spawn-fn %1 color side %2 [0 0 1]
                (partial dat/crowd-ai-next pts)
                dat/crowd-ai-in
              )
              recruits
              (cycle pts)
            )
          )
        )
      )
      all-spawn (fn [info loc]
        (fn [spawn-fn]
          (concat
            ((crowd-spawn loc) spawn-fn)
            ((guard-spawn info loc) spawn-fn)
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
        :pos (-> "castle_red" world-markers nav/marker-pos)
        :rot [0 (-> "castle_red" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
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
        :pos (-> "castle_blue" world-markers nav/marker-pos)
        :rot [0 (-> "castle_blue" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
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
        :name :castle-desert
        :side :blue
        :color [219/255 154/255 89/255 1]
        :pos (-> "castle_sand" world-markers nav/marker-pos)
        :rot [0 (-> "castle_sand" world-markers nav/marker-rot-y) 0]
        :scale [1 1 1]
        :spawn (partial all-spawn [
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
