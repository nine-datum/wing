(ns wing.server
  [:import
    [java.io DataInputStream DataOutputStream BufferedInputStream]
    [java.net ServerSocket]
  ]
)

(defn start-server [port]
  (future
    (let [
        serv (doto (ServerSocket. port))
        sock (.accept serv)
        in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
        line (atom "")
      ]
      (println "server started, client connected")
      (while (not= @line "end")
        (reset! line (.readUTF in))
        (println @line)
      )
      (println "server closed")
      (.close in)
      (.close sock)
      (.close serv)
    )
  )
)
