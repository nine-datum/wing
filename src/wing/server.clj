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
(def server-res (atom nil))

(defn running? [] @active?)

(defn close-server []
  (reset! active? false)
  (swap! server-res #(when (-> % nil? not)
    (.close %)
  ))
)

(defn handle-client [sock clients]
  (future
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          last (atom "")
        ]
        (println "client connected : " name)
        (while (and active? (not= @last "end"))
          (let [
              n (.readUTF in)
              l (.readUTF in)
            ]
            (reset! last l)
            (println "a message from " n " : " l)
            (doseq [c (-> @clients set (disj sock))]
              (-> c .getOutputStream DataOutputStream.
                (doto
                  (.writeUTF n)
                  (.writeUTF l)
                )
              )
            )
          )
        )
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
        (reset! server-res serv)
        (println "server started")
        (while active? (let [
            new-cl (.accept serv)
          ]
          (swap! clients (partial cons new-cl))
          (handle-client new-cl clients)
        ))
        (println "server closed")
        (close-server)
      )
      (catch Throwable e (println "Starting server error : " e))
    )
  )
  nil
)
