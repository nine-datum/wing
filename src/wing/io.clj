(ns wing.io
  [:require
    [clojure.java.io :refer [make-parents]]
  ]
)

(def profile-path "user/profile")

(defn load-profile []
  (-> profile-path java.io.File. .exists (when (-> profile-path slurp read-string)))
)

(defn save-profile [p]
  (make-parents profile-path)
  (spit profile-path (pr-str p))
)
