(ns nine-clj.mac)

(defmacro --> [e & es] (reduce #(-> %2 seq? (if cons list) (apply [%1 %2])) e es))

(defmacro impl [typ met params & exprs]
  `(proxy [~typ] [] (~met [~@params] ~@exprs))
)
