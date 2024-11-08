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
      NodeReader
      StringReader
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

(defmacro node-reader [n & exprs]
  `(proxy [NodeReader] [] (read [~n] ~@exprs))
)

(defmacro string-reader [n & exprs]
  `(proxy [StringReader] [] (read [~n] ~@exprs))
)

(defn geom-markers [storage file]
  (let [
      root (->> file (.open storage) ColladaNode/fromFile)
      res (atom ())
    ]
    (.children root "COLLADA" (node-reader cld
    (.children cld "library_visual_scenes" (node-reader scenes
    (.children scenes "visual_scene" (node-reader scene
    (.children scene "node" (node-reader node
    (.attribute node "name" (string-reader name
    (.children node "matrix" (node-reader matrix
    (.content matrix (string-reader nums (swap! res #(cons [name nums] %))))))))))))))))
    (mapv (fn [[name nums]] [name
      (as-> nums n
        (clojure.string/split n #" ")
        (mapv #(Float/valueOf %) n)
        (Buffer/of n)
        (Matrix4f/from_COLLADA_Buffer n 0)
      )]) @res)
  )
)

(defn geom-markers-map [storage file]
  (->> (geom-markers storage file) (apply concat) (apply hash-map))
)
