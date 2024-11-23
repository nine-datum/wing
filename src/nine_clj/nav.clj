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
      pts (->> pts
        (take-while identity)
        (map #(.transformPoint % (math/vec3f 0 0 0)))
        vec
      )
    ]
    pts
  )
)

(defn path [pts from to]
  (defn search [ps way end dst]
    (cond
      (or (empty? ps) (-> way last (= end))) [[ way dst ]]
      :else (->> ps
        (map #(search
          (disj ps %)
          (conj way %)
          end
          (->> way last (mapv - %) mat/length (+ dst)))
        )
        (apply concat)
      )
    )
  )
  (cond
    (empty? pts) (-> (mapv - to from) (assoc 1 0) math/normalize)
    :else (let [
        ways (search (set (conj pts to)) [from] to 0)
        ways (sort-by second ways)
      ]
      (first ways)
    )
  )
)
