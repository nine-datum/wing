(ns nine-clj.prof
  (:import
    [javax.swing
      JFrame
      JPanel
    ]
    [java.awt
      Color
    ]
    [java.awt.event
      WindowAdapter
    ]
  )
)

(defn current-time [] (. System nanoTime))

(def time-map (atom {}))
(def last-map (atom {}))

(defmacro measure [expr]
  `(let [start# (current-time) res# ~expr end# (current-time)]
    (vector res# (- end# start#))
  )
)

(defmacro pmeasure [name expr]
  `(let [[r# t#] (measure ~expr)]
    (println (str ~name " took " (double (/ t# 1000000)) " ms"))
    r#
  )
)

(defn reset [] (reset! last-map @time-map) (reset! time-map {}))

(defn cover-nil [x] (if (nil? x) 0 x))

(defmacro profile [name expr]
  `(do
    (let [[res# time#] (measure ~expr)]
      (swap! time-map #(update % ~name (comp (partial + time#) cover-nil)))
      res#
    )
  )
)

(defn paint-text [graphics text x y color]
  (let [[r g b] color]
    (.setColor graphics (Color. 255 255 255 255))
    (.drawString graphics text x y)
  )
)

(defn paint-time [g w h]
  (let [
      ms @last-map
      one 1000000000.0
      main-loop (get ms :main-loop one)
      ms (dissoc ms :main-loop)
      proc (fn [v] (-> v (/ main-loop) (* 100) int))
      ;sum (proc (apply + (vals ms)))
      ;ms (assoc ms :other (- 100 sum))
      mseq (sort-by (comp - second) (seq ms))
      mseq (map #(update % 1 (fn [v] (str (% 0) " took " (proc v) "%"))) mseq)
      mseq (cons [:fps (str "fps = " (->> main-loop (/ one) int))] mseq)
    ]
    (doseq [[[k v] i] (map vector mseq (range))]
      (paint-text g v 10 (* 25 (inc i)) [1 0 0])
    )
  )
)

(defn window [x y w h]
  (let [
      panel (proxy [JPanel][]
        (paint [g]
          (.setColor g Color/BLACK)
          (.fillRect g 0 0 w h)
          (paint-time g w h)
        )
      )
      panel (doto panel
        (.setSize w h)
      )
      closed? (atom false)
      frame (doto (JFrame.)
        (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
        (.addWindowListener
          (proxy [WindowAdapter] []
            (windowClosing [e]
              (reset! closed? true)
            )
          )
        )
        (.setSize w h)
        (.setLocation x y)
        (.add panel)
      )
    ]
    (.setVisible frame true)
    (future
      (while (not @closed?)
        (Thread/sleep 500)
        (.repaint panel)
      )
    )
  )
)