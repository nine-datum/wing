(defproject nine-clj "0.1.0-SNAPSHOT"
  :description "Clojure OpenGL 3D graphics library"
  :url "http://github.com/Taqmuraz/nine-clj"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [
    [org.clojure/clojure "1.11.1"]
  ]
  :resource-paths [
    "lib/nine-1.0.jar"
    "lib/lib/jar.jar"
    "lib/lib/jar1.jar"
    "lib/lib/jar2.jar"
    "lib/lib/jar3.jar"
    "lib/lib/jar4.jar"
    "lib/lib/jar5.jar"
    "lib/lib/jar6.jar"
    "lib/lib/jar7.jar"
    "lib/lib/jar8.jar"
    "lib/lib/jar9.jar"
    "lib/lib/jar10.jar"
    "lib/lib/jar11.jar"
    "lib/lib/jar12.jar"
    "lib/lib/jar13.jar"
    "lib/lib/jar14.jar"
    "lib/lib/jar15.jar"
    "lib/lib/jar16.jar"
    "lib/lib/jar17.jar"
  ]
  :main ^:skip-aot nine-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
