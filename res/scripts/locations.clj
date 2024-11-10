(require
  '[nine-clj.datum :as dat]
  '[nine-clj.math :as math]
)
(fn [dev all-presets]
  (let [
      marker (fn [loc marker-name]
        (.mul (loc :mat) (-> loc :name all-presets :markers (get marker-name)))
      )
      marker-pos (fn [loc marker-name]
        (-> (marker loc marker-name) (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
      )
      marker-look (fn [loc marker-name]
        (-> (marker loc marker-name) (.transformVector (math/vec3f 0 0 1)) math/floats-from-vec3f math/normalize)
      )
      location (fn [& args]
        (let [
            h (apply hash-map args)
            { :keys [name pos rot scale] } h
            mat (math/transform pos rot scale)
            h (assoc h :mat mat)
            entry (->> (get h :entry "entry"))
          ]
          {
            :name name
            :preset (all-presets name)
            :models (-> name all-presets :models)
            :entry-pos (marker-pos h entry)
            :entry-look (marker-look h entry)
            :pos pos
            :rot rot
            :scale scale
            :spawn ((h :spawn) h)
          }
        )
      )
      guard-spawn (fn [info loc]
        (let [
            { :keys [color side] } loc
            ps (partition 2 info)
            ps (mapv (fn [[kind mark]] (vector kind color side (marker-pos loc mark) (marker-look loc mark))) ps)
          ]
          (fn [spawn-fn]
            (mapv #(apply spawn-fn %) ps)
          )
        )
      )
    ]
    [
      (location
        :name :castle
        :side :red
        :color [1 0 0 1]
        :pos [38 200 655]
        :rot [0 (-> Math/PI (/ 2) -) 0]
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
      )
    ]
  )
)
