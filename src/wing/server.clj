(ns wing.server
  [:require
    [wing.client :as client]
  ]
  [:import
    [java.io DataInputStream DataOutputStream BufferedInputStream]
    [java.net ServerSocket]
  ]
)

(def active? (atom false))

(defn close-server []
  (reset! active? false)
)

(defn handle-client [sock clients]
  (future
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          line (atom "")
          name (.readUTF in)
        ]
        (println "client connected : " name)
        (while (and @active? (not= "end" (reset! line (.readUTF in))))
          (swap! line (fn [l]
            (println "a message from " name " : " l)
            (client/accept name (read-string l))
            (doseq [c (-> @clients set (disj sock))]
              (-> c .getOutputStream DataOutputStream.
                (doto
                  (.writeUTF name)
                  (.writeUTF l)
                )
              )
            )
          ))
        )
        (close-server)
        (println "client disconnected :" name)
        (.close in)
        (.close sock)
      )
      (catch Throwable e (println "Handling client error : " e))
    )
  )
  nil
)

(defn start-server [port]
  (future
    (try
      (let [
          serv (ServerSocket. port)
          clients (atom ())
        ]
        (reset! active? true)
        (println "server started")
        (while active? (let [
            new-cl (.accept serv)
          ]
          (swap! clients (partial cons new-cl))
          (handle-client new-cl clients)
        ))
        (println "server closed")
        (.close serv)
      )
      (catch Throwable e (println "Starting server error : " e))
    )
  )
  nil
)
