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
    ]
    (.setColor g (. Color WHITE))
    (.setFont g font)
    (doseq [i (range 256)]
      (let [
          s (-> i char str)
          tl (TextLayout. s font frc)
          b (.getBounds tl)
          px (.getMinX b)
          py (.getMinY b)
          sx 1
          sy 1
          x (* fs (rem i 16))
          y (* fs (quot i 16))
          t (.getTransform g)
        ]
        (.drawRect g x y fs fs)
        (.translate g x y)
        (.translate g (- px) (- py))
        (.scale g sx sy)
        (.drawString g s 0 0)
        (.setTransform g t)
      )
    )
    (.dispose g)
    img
  )
)

(defn save-text-png [img file]
  (javax.imageio.ImageIO/write img "png" (java.io.File. file))
)

(defn text-image-cached [font]
  (let [
      file (str "res/fonts/" (.getName font) "-" (.getSize font) ".png")
    ]
    (cond
      (-> file File. .exists) (javax.imageio.ImageIO/read (File. file))
      :else (doto (text-image font) (save-text-png file))
    )
  )
)

(defn text-geom [gl text]
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
            s (int (nth text i))
            x (/ (rem s 16) 16)
            y (/ (quot s 16) 16)
            y (- 1 y 1/16)
          ]
          (mapv #(mapv + (map (partial * 1/16) %) [x y]) buv)
        )
      )
      r (* 6 l)
      bx (fn [i] (mapv #(mapv / (mapv + % [i 0 0]) [l 1 1]) bvs))
      vs (apply concat (map bx (range l)))
      uvs (apply concat (map uv (range l)))
      nrs (mapv (constantly [0 0 1]) (range r))
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