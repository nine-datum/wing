(ns nine-clj.geom
  [:import
    [nine.buffer
      Buffer
      CachedBuffer
    ]
    [nine.geometry.collada
      ColladaGeometryParser
      ColladaBasicGeometryParser
      ColladaNode
      BuffersReader
      BufferMapping
    ]
  ]
)

(defn read-geom [storage file]
  (let [
      node (. ColladaNode fromFile (.open storage file))
      g (ColladaBasicGeometryParser.)
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
            (swap! res (partial cons { :vertex rv :index ri }))
          )
        )
      )
    )
    @res
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