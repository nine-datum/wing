(ns nine-clj.core-test
  (:require
    [clojure.test :refer :all]
    [nine-clj.core :refer :all]
    [nine-clj.math :as math]
  )
)

(deftest clock-test
  (testing "Clock functions"
    (is (=
      (->> 360 range (map (comp double (partial apply math/clock) math/clock-xy)))
      (range 360)
    ))
  )
)