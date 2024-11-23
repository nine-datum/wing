(ns nine-clj.nav
  [:require
    [nine-clj.math :as math]
    [clojure.core.matrix :as mat]
  ]
)

(defn location-nav [location]
  (let [
      names (map #(str "nav_") (range))
      pts (-> location :preset :markers (map names))
      pts (->> ms
        (take-while identity)
        (map #(.transformPoint % (math/vec3 0 0 0)))
        vec
      )
    ]
    pts
  )
)

(defn from-to-in [pts from to]
  (cond
    (empty? pts) (-> (mapv - to from) (assoc 1 0) math/normalize)
    :else (let [
        cls (->> pts
          (filter #(-> % math/length (> 1)))
          (sort-by #(-> % (mapv - from) mat/length))
          first
        )
        cls (if (nil? cls) to cls)
      ]
      (-> (mapv - cls from) (assoc 1 0) math/normalize)
    )
  )
)
