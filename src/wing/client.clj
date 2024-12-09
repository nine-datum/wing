(ns wing.client
  [:import
    [java.io DataInputStream DataOutputStream BufferedReader]
    [java.net Socket]
  ]
)

(def active? (atom true))

(defn start-client [addr port]
  (future
    (let [
        sock (Socket. addr port)
        out (-> sock .getOutputStream DataOutputStream.)
      ]
      (println "client started")
      (doseq [i (range 10)] (.writeUTF out "hellou"))
      (.writeUTF out "end")
      (println "client closed")
      (.close out)
      (.close sock)
    )
  )
)
