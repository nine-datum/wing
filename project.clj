(defproject org.clojars.taqmuraz/nine-clj "0.1.2"
  :description "Clojure OpenGL 3D graphics library"
  :url "http://github.com/Taqmuraz/nine-clj"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.11.1"]
    [net.mikera/core.matrix "0.63.0"]
    [io.github.taqmuraz/nine "1.2.25"]
    [cz.advel.jbullet/jbullet "20101010-1"]
  ]
  ;:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]
  :main ^:skip-aot nine-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
)