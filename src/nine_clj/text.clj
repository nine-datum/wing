(ns nine-clj.text
  (:import
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

(defn default-font [size] (font "Serif" size))

(defn text-image [text font]
  (let
    [
      frc (FontRenderContext. nil
        (. RenderingHints VALUE_TEXT_ANTIALIAS_ON)
        (. RenderingHints VALUE_FRACTIONALMETRICS_OFF)
      )
      tl (TextLayout. text font frc)
      b (.getBounds tl)
      w (.getWidth b)
      h (.getHeight b)
      img (BufferedImage. w h (. BufferedImage TYPE_INT_ARGB))
      g (.createGraphics img)
    ]
    (.setFont g font)
    (.setColor g (. Color WHITE))
    (.drawString g text 0 (int h))
    (.dispose g)
    img
  )
)