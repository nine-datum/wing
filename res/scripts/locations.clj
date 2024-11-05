(fn [dev all-presets]
  (let [
      location (fn [sym pos rot scale] { :model (-> sym all-presets :model) :pos pos :rot rot :scale scale })
    ]
    [
      (location :castle [38 200 655] [0 (-> Math/PI (/ 2) -) 0] [1 1 1])
    ]
  )
)
