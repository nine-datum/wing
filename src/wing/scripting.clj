(ns wing.scripting)

(defn read-file [file]
  (with-open [r (clojure.java.io/reader file)]
    (let [
        p (java.io.PushbackReader. r)
        ex (loop [ex []]
          (let [e (read p false nil)]
            (cond (nil? e) ex :else (recur (conj ex e)))
          )
        )
        res (binding [*ns* (-> file java.io.File. .getName symbol create-ns)]
          (clojure.core/refer-clojure)
          (last (mapv eval ex))
        )
      ]
      res
    )
  )
)