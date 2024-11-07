(fn [dev all-presets]
  (let [
      location (fn [sym pos entry rot scale] {
        :preset (all-presets sym)
        :models (-> sym all-presets :models)
        :entry entry
        :pos pos
        :rot rot
        :scale scale
      }
    )
    ]
    [
      (location :castle [38 200 655] [88 215 655] [0 (-> Math/PI (/ 2) -) 0] [1 1 1])
    ]
  )
)
