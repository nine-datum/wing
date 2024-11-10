(ns nine-clj.part
  [:require
    [nine-clj.math :as math]
    [clojure.core.matrix :as mat]
  ]
  [:import
    [nine.buffer Buffer]
  ]
)

(defn part-geom [gl num]
  (let [
      bv [
        [0 0 0]
        [0 1 0]
        [1 1 0]

        [1 1 0]
        [1 0 0]
        [0 0 0]

        [0 0 0]
        [1 0 0]
        [1 1 0]

        [1 1 0]
        [0 1 0]
        [0 0 0]
      ]
      buv [
        [0 0]
        [0 1]
        [1 1]

        [1 1]
        [1 0]
        [0 0]

        [0 0]
        [1 0]
        [1 1]

        [1 1]
        [0 1]
        [0 0]
      ]
      bn [
        [0 0 1]
        [0 0 1]
        [0 0 1]

        [0 0 1]
        [0 0 1]
        [0 0 1]

        [0 0 -1]
        [0 0 -1]
        [0 0 -1]

        [0 0 -1]
        [0 0 -1]
        [0 0 -1]
      ]
      r (* num 12)
      vs (->> bv cycle (mapv #(mapv + [0 0 (quot %1 12)] %2) (range r)))
      uvs (->> buv cycle (take r))
      nrs (->> bn cycle (take r))
      to-float (partial map float)
      buf (comp vec to-float (partial apply concat))
      geom (-> gl
        (.vao (. Buffer range r))
        (.attribute 3 (. Buffer of (buf vs)))
        (.attribute 2 (. Buffer of (buf uvs)))
        (.attribute 3 (. Buffer of (buf nrs)))
        (.drawing)
      )
    ]
    geom
  )
)
