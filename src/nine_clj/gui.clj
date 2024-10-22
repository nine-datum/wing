(ns nine-clj.gui
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.text :as text]
  ]
)

(defn gui-asset [gl storage mouse]
  (let [
      image-shader (graph/load-shader gl storage "res/shaders/image_vertex.glsl" "res/shaders/image_fragment.glsl")
      text-shader image-shader
      text-asset (graph/load-text-asset gl (text/load-font "res/fonts/ubuntu.ttf" 50))
      button-image (graph/load-image gl storage "res/images/button.png")
    ]
    {
      :mouse mouse
      :image-shader image-shader
      :text-shader text-shader
      :text-asset text-asset
      :button-image button-image
    }
  )
)

(defn button [asset label x y w h]
  (let [
      [mx my] ((asset :mouse) :pos)
      [ex ey] (map + [x y] [w h])
      hovered (and (<= x mx ex) (<= y my ey))
      img-color (if hovered [0 1 0 1] [0.5 0.5 0.5 1])
      txt-color (if hovered [1 1 1 1] [0 1 0 1])
    ]
    (graph/image (asset :button-image) (asset :image-shader) x y w h img-color)
    (graph/text (asset :text-asset) (asset :text-shader) label (+ x (/ w 5)) (+ y (/ h 4)) w (* h 3/4) txt-color)
    hovered
  )
)