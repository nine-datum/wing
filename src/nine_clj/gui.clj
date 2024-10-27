(ns nine-clj.gui
  [:require
    [nine-clj.graph :as graph]
    [nine-clj.text :as text]
  ]
)

(defn gui-asset [dev]
  (let [
      { :keys [gl storage mouse width height] } dev
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
      :width width
      :height height
    }
  )
)

(defn image [asset layout image x y w h color]
  (let [
      [x y w h] (layout asset x y w h)
    ]
    (graph/image image (asset :image-shader) x y w h color)
  )
)

(defn button [asset layout label x y w h]
  (let [
      [x y w h] (layout asset x y w h)
      [mx my] ((asset :mouse) :pos)
      [ex ey] (map + [x y] [w h])
      hovered (and (<= x mx ex) (<= y my ey))
      pressed (and hovered ((asset :mouse) :left-down))
      img-color (cond
        pressed [1 1 1 1]
        hovered [0 1 0 1]
        :else [0.5 0.5 0.5 1]
      )
      txt-color (cond
        pressed [1 1 0 1]
        hovered [1 1 1 1]
        :else [0 1 0 1]
      )
    ]
    (graph/image (asset :button-image) (asset :image-shader) x y w h img-color)
    (graph/text (asset :text-asset) (asset :text-shader) label (+ x (/ w 5)) (+ y (/ h 4)) w (* h 3/4) txt-color)
    pressed
  )
)

(defn aspect-fit-layout [asset x y w h]
  (let [
      sw (-> asset :width list eval)
      sh (-> asset :height list eval)
      m (cond (> sw sh) [(/ sh sw) 1] :else [1 (/ sw sh)])
    ]
    (mapv * [x y w h] (cycle m))
  )
)