(ns nine-clj.geom
  [:import
    [nine.buffer
      Buffer
      CachedBuffer
    ]
    [nine.math Matrix4f]
    [nine.geometry.collada
      ColladaGeometryParser
      ColladaBasicGeometryParser
      ColladaBasicVisualSceneParser
      ColladaNode
      BuffersReader
      ColladaVisualSceneReader
      BufferMapping
    ]
  ]
)

(defn read-geom [storage file]
  (let [
      node (. ColladaNode fromFile (.open storage file))
      g (ColladaBasicGeometryParser.)
      vs (ColladaBasicVisualSceneParser.)
      res (atom (list))
    ]
    (.read g node
      (proxy [BuffersReader] []
        (read [s m floats ints]
          (let [
              v (.map floats "VERTEX")
              i (.map ints "INDEX")
              v (.fromRightToLeftHanded v)
              rv (-> v .toList vec)
              ri (-> i .toList vec)
            ]
            (swap! res (partial cons { :source (str "#" s) :vertex rv :index ri :root (. Matrix4f identity) }))
          )
        )
      )
    )
    (swap! res (partial group-by :source))
    (.read vs node
      (proxy [ColladaVisualSceneReader] []
        (read [id root]
          (cond
            (contains? @res id) (swap! res
              #(update % id
                (fn [geoms]
                  (mapv (fn [geom] (assoc geom :root root)) geoms)
                )
              )
            )
          )
        )
      )
    )
    (->> @res vals (apply concat))
  )
)

(defn geom-offset-parser [source-pred offset]
  (proxy [ColladaGeometryParser] []
    (read [node reader]
      (.read (ColladaBasicGeometryParser.) node
        (proxy [BuffersReader] []
          (read [source mat floats ints]
            (.read reader source mat
              (if (source-pred source)
                (proxy [BufferMapping] []
                  (map [semantic]
                    (let [sr (.map floats semantic)]
                      (case semantic "VERTEX"
                        (CachedBuffer. (proxy [Buffer] []
                          (length [] (.length sr))
                          (at [i]
                            ((comp float +) (.at sr i) (offset (mod i 3)))
                          )
                        ))
                        sr
                      )
                    )
                  )
                )
                floats
              )
              ints
            )
          )
        )
      )
    )
  )
)

(defn scale-geom [verts scale]
  (mapv (fn [f i] (* f (scale (mod i 3)))) verts (range))
)