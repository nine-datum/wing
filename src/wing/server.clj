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
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          line (atom "")
          name (.readUTF in)
        ]
        (println "client connected : " name)
        (while (and @active? (not= @line "end"))
          (reset! line (.readUTF in))
          (println "a message from " name " : " @line)
        )
        (close-server)
        (println "client disconnected :" name)
        (.close in)
        (.close sock)
      )
      (catch Throwable e (println e))
    )
  )
  nil
)

(defn start-server [port]
  (future
    (try
      (let [
          serv (ServerSocket. port)
        ]
        (reset! active? true)
        (println "server started")
        (while active? (handle-client (.accept serv)))
        (println "server closed")
        (.close serv)
      )
      (catch Throwable e (println e))
    )
  )
  nil
)
