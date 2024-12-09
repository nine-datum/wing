(ns wing.client
  [:import
    [java.io DataInputStream DataOutputStream BufferedReader]
    [java.net Socket]
  ]
)

(def active? (atom false))
(def rate 1)

(defn start-client [addr port name]
  (future
    (let [
        sock (Socket. addr port)
        out (-> sock .getOutputStream DataOutputStream.)
      ]
      (reset! active? true)
      (println "client started")
      (.writeUTF out name)
      (while @active?
        (.writeUTF out "hello, niga")
        (Thread/sleep (/ 1 rate 1/1000))
      )
      (.writeUTF out "end")
      (println "client closed")
      (.close out)
      (.close sock)
    )
  )
)

(defn close-client []
  (reset! active? false)
)
