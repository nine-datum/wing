(ns wing.core-test
  (:require
    [clojure.test :refer :all]
    [wing.core :refer :all]
    [wing.math :as math]
    [clojure.math :as cmath]
    [wing.nav :as nav]
  )
)

(deftest clock-test
  (testing "Clock functions"
    (is (=
      (->> 360 range
        (map (partial * 1/180 Math/PI))
        (map math/clock-xy)
        (map (partial apply math/clock))
        (map double)
        (map #(/ % 1/180 Math/PI))
        (map cmath/round)
        (map #(mod % 360))
      )
      (range 360)
    ))
  )
)

(deftest nav-test
  (testing "Path finding function"
    (is (=
      (nav/path [[0 0 0] [-1 0 0] [1 0 0] [0 0 6] [0 0 5]] [0 0 1] [0 0 10])
      [[[0 0 1] [0 0 10]] 9.0]
    ))
  )
)
