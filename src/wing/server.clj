(ns wing.server
  [:import
    [java.io DataInputStream DataOutputStream BufferedInputStream]
    [java.net ServerSocket]
  ]
)

(def active? (atom false))

(defn close-server []
  (reset! active? false)
)

(defn handle-client [sock]
  (future
    (let [
        in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
        line (atom "")
        name (.readUTF in)
      ]
      (while (and @active? (not= @line "end"))
        (reset! line (.readUTF in))
        (println "a message from " name " : " @line)
      )
      (close-server)
      (println "client " name " disconnected")
      (.close in)
      (.close sock)
    )
  )
  nil
)

(defn start-server [port]
  (future
    (let [
        serv (doto (ServerSocket. port))
      ]
      (reset! active? true)
      (while active? (handle-client (.accept serv)))
      (println "server started")
      (.close serv)
    )
  )
  nil
)
