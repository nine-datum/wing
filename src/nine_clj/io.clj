(ns nine-clj.io
  [:require
    [nine-clj.datum :as dat]
    [nine-clj.scenes.world :as world]
    [clojure.java.io :refer [make-parents]]
    [zprint.core :as zprint]
  ]
)

(defn data-to-spawn [data]
  (fn [phys-world presets]
    (let [
        horse (presets :horse)
        ship (presets :ship)
        units (data :units)
      ]
      (mapv (fn [unit]
        (let [
            { :keys [kind pos look color side army] } unit
            rider (presets kind)
          ]
          (assoc (world/load-horse phys-world horse rider ship color side pos look)
            :army army
          )
        )
      ) units)
    )
  )
)

(defn unit-to-data [unit]
  (assoc (select-keys unit [:pos :look :side :color :kind :army])
    :kind (-> unit :preset :name)
  )
)

(defn location-to-data [loc]
  (select-keys loc [:id :name :army :recruits :color :side])
)

(defn data-to-location [dev res data]
  (-> res :world-locations (get (data :id)) (merge data))
)

(defn state-to-data [state]
  (let [
      { :keys [player non-players locations] } state
      units (as-> player r (cons r non-players) (map unit-to-data r) (list* r))
      locations (-> locations (update-vals location-to-data))
    ]
    (assoc (select-keys state [ :campos :camrot :camrot-xy :camdist ])
      :units units
      :locations locations
    )
  )
)

(defn save-game [state file]
  (make-parents file)
  (as-> state x (state-to-data x) (zprint/zprint-str x { :map { :indent 2 :comma? false :force-nl? true } } ) (spit file x))
)

(defn load-game [dev res file]
  (let [
      data (-> file slurp read-string)
      state (select-keys data [:campos :camrot :camrot-xy :camdist])
      spawn (data-to-spawn data)
      locations (data :locations)
      locations (cond
        (empty? locations) (res :world-locations)
        :else (-> data :locations (update-vals (partial data-to-location dev res)))
      )
      state (assoc state :locations locations)
    ]
    (world/world-load-setup dev res state spawn)
  )
)
