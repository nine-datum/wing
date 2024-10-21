(ns nine-clj.gui
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.text :as text]
  ]
)

(defn gui-asset [gl storage]
  (let [
      image-shader (graph/load-shader gl storage "res/shaders/image_vertex.glsl" "res/shaders/image_fragment.glsl")
      text-shader image-shader
      text-asset (graph/load-text-asset gl (text/load-font "res/fonts/ubuntu.ttf" 50))
      button-image (graph/load-image gl storage "res/images/button.png")
    ]
    {
      :image-shader image-shader
      :text-shader text-shader
      :text-asset text-asset
      :button-image button-image
    }
  )
)

(defn button [asset label x y w h]
  (graph/image (asset :button-image) (asset :image-shader) x y w h)
  (graph/text (asset :text-asset) (asset :text-shader) label (+ x (/ w 5)) (+ y (/ h 4)) w (* h 3/4))
)