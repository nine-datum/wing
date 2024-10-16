(ns nine-clj.text
  (:import
    [nine.buffer
      Buffer
    ]
    [java.awt.font
      FontRenderContext
      TextLayout
    ]
    [java.awt
      Font
      Color
      RenderingHints
    ]
    [java.awt.geom
      AffineTransform
    ]
    [java.awt.image
      BufferedImage
    ]
    [java.io
      File
    ]
    [javax.imageio
      ImageIO
    ]
  )
)

(defn font
  ([name size] (Font. name (. Font PLAIN) size))
  ([name size style] (Font. name style size))
)

(defn load-font [file size] (.deriveFont (. Font createFont Font/PLAIN (File. file)) size))

(defn default-font [size] (font "Serif" size))

(defn text-image [font]
  (let
    [
      frc (FontRenderContext. (AffineTransform.)
        (. RenderingHints VALUE_TEXT_ANTIALIAS_ON)
        (. RenderingHints VALUE_FRACTIONALMETRICS_OFF)
      )
      fs (.getSize font)
      img (BufferedImage. (* 16 fs) (* 16 fs) (. BufferedImage TYPE_INT_ARGB))
      g (.createGraphics img)
      char-nums (mapv
        (fn [i]
          (if (<= 192 i 256) (-> i (- 192) (+ 1040)) i) ; rendering cyrillic after 192
        )
        (range 256)
      )
      chars (mapv (comp str char) char-nums)
      image-rects (mapv
        (fn [s]
          (let [
              tl (TextLayout. s font frc)
              b (.getBounds tl)
              px (.getMinX b)
              py (.getMinY b)
              pw (.getWidth b)
              ph (.getHeight b)
            ]
            [px py pw ph]
          )
        )
        chars
      )
      char-rects (mapv #(mapv / % (repeat fs)) image-rects)
    ]
    (.setColor g (. Color WHITE))
    (.setFont g font)
    (doseq [i (range 256)]
      (let [
          n (char-nums i)
          s (chars i)
          [px py pw ph] (image-rects i)
          sx 0.9
          sy 0.9
          x (* fs (rem i 16))
          y (* fs (quot i 16))
          t (.getTransform g)
        ]
        (.translate g x y)
        (.translate g (- px) (- py))
        (.scale g sx sy)
        (.drawString g s 0 0)
        (.setTransform g t)
      )
    )
    (.dispose g)
    { :img img :rects char-rects }
  )
)

(defn save-text-png [img file]
  (javax.imageio.ImageIO/write img "png" (java.io.File. file))
)

(defn text-image-cached
  ([font] (text-image-cached font false))
  ([font reset-cache]
    (let [
        file (str "res/fonts/" (.getName font) "-" (.getSize font))
        img-file (str file ".png")
        rects-file (str file "-rects" ".txt")
      ]
      (cond
        (and
          (false? reset-cache)
          (-> img-file File. .exists)
          (-> rects-file File. .exists)
        ) {
          :img (javax.imageio.ImageIO/read (File. img-file))
          :rects (-> rects-file slurp read-string)
        }
        :else (doto (text-image font)
          (-> :img (save-text-png img-file))
          (->> :rects str (spit rects-file))
        )
      )
    )
  )
)

(defn table-index [sym]
  (let [
      i (int sym)
    ]
    (if (<= 1040 i 1104) (-> i (- 1040) (+ 192)) i) ; masking cyrillic
  )
)

(defn text-geom [gl rects text]
  (let [
      l (.length text)
      bvs [
        [0 0 0]
        [1 0 0]
        [1 1 0]

        [1 1 0]
        [0 1 0]
        [0 0 0]
      ]
      buv [
        [0 0]
        [1 0]
        [1 1]

        [1 1]
        [0 1]
        [0 0]
      ]
      uv (fn [i]
        (let [
            s (table-index (nth text i))
            x (/ (rem s 16) 16)
            y (/ (quot s 16) 16)
            y (- 1 y 1/16)
          ]
          (mapv #(mapv + (map (partial * 1/16) %) [x y]) buv)
        )
      )
      r (* 6 l)
      rects (mapv (comp rects table-index) text)
      offsets (mapv (comp #(conj % 0) vec (partial take 2)) rects)
      sizes (mapv (comp #(conj % 1) vec (partial drop 2)) rects)
      bx (fn [i]
        (mapv #(
            ->> %
            ;(mapv * (sizes i))
            (mapv + [i 0 0] (mapv - (offsets i)))
            (mapv * [(/ 1 l) 1 1])
          ) bvs
        )
      )
      nfs (constantly (repeat 6 [0 0 1]))
      discard-ws (fn [f] (fn [i] (cond (= \space (-> text (nth i))) [] :else (f i))))
      [bx uv nfs] (map discard-ws [bx uv nfs])
      vs (apply concat (map bx (range l)))
      uvs (apply concat (map uv (range l)))
      nrs (apply concat (map nfs (range l)))
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