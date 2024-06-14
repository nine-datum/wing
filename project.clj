(defproject org.clojars.taqmuraz/nine-clj "0.1.1-SNAPSHOT"
  :description "Clojure OpenGL 3D graphics library"
  :url "http://github.com/Taqmuraz/nine-clj"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.11.1"]
    [org.clojars.taqmuraz/nine "1.0"]
  ]
  :main ^:skip-aot nine-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
)