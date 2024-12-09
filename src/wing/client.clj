(ns wing.client
  [:import
    [java.io DataInputStream DataOutputStream BufferedReader]
    [java.net Socket]
  ]
)

(defn start-client [addr port]
  (future
    (let [
        sock (Socket. addr port)
        out (-> sock .getOutputStream DataOutputStream.)
      ]
      (println "client started")
      (.writeUTF out "hellou")
      (.close out)
      (.close sock)
    )
  )
)
