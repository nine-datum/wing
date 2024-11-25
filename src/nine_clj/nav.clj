(ns nine-clj.nav
  [:require
    [nine-clj.math :as math]
    [clojure.core.matrix :as mat]
  ]
)

(defn marker [loc marker-name]
  (.mul (loc :mat) (-> loc :preset :markers (get marker-name)))
)
(defn marker-pos [m]
  (-> m (.transformPoint (math/vec3f 0 0 0)) math/floats-from-vec3f)
)
(defn marker-look [m]
  (-> m (.transformVector (math/vec3f 0 0 1)) math/floats-from-vec3f math/normalize)
)
(defn marker-rot-y [m]
  (as-> m v (marker-look v) (map v [0 2]) (apply math/clock v))
)

(defn location-pts [location name]
  (let [
      names (map #(str name "_" %) (range))
      markers (-> location :preset :markers)
      pts (->> names
        (take-while #(contains? markers %))
        (map #(marker location %))
        (map marker-pos)
        vec
      )
    ]
    pts
  )
)

(defn location-nav [location] (location-pts location "nav"))
(defn location-spots [location] (location-pts location "int"))

(defn path [pts from to]
  (defn search [ps way end dst]
    (cond
      (or (empty? ps) (-> way last (= end))) [[ way dst ]]
      :else (let [
          l (last way)
          p (first (sort-by #(mat/length (mapv - % l)) ps))
        ]
        (search
          (disj ps p)
          (conj way p)
          end
          (->> way last (mapv - p) mat/length (+ dst))
        )
      )
    )
  )
  (cond
    (empty? pts) [from to]
    :else (let [
        ways (search (set (conj pts to)) [from] to 0)
        ways (sort-by second ways)
      ]
      (first ways)
    )
  )
)
