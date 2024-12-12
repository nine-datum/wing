(ns wing.server
  [:require
    [wing.client :as client]
  ]
  [:import
    [java.io DataInputStream DataOutputStream BufferedInputStream]
    [java.net ServerSocket DatagramPacket DatagramSocket]
  ]
)

(def active? (atom false))

(defn running? [] @active?)

(defn close-server []
  (reset! active? false)
)

(defn handle-client [sock clients]
  (future
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          last (atom "")
        ]
        (println "client connected : " name ", clients total : " (count @clients))
        (while (and active? (-> sock .isClosed not) (not= @last "end"))
          (let [
              l (.readUTF in)
            ]
            (reset! last l)
            (doseq [c (-> @clients set (disj sock))]
              (-> c .getOutputStream DataOutputStream.
                (.writeUTF l)
              )
            )
          )
        )
        (println "client disconnected :" name ", last message was :" @last)
        (.close in)
        (.close sock)
      )
      (catch Throwable e (println "Handling client error : " e))
    )
  )
  nil
)

(declare start-udp-listener)

(defn start-server [port udp-port name]
  (reset! active? true)
  (future
    (try
      (let [
          serv (ServerSocket. port)
          clients (atom ())
        ]
        (start-udp-listener udp-port name)
        (println "server started")
        (future
          (try
            (while active? (let [
                new-cl (.accept serv)
              ]
              (swap! clients (fn [cs] (cons new-cl (filterv #(-> % .isClosed not) cs))))
              (handle-client new-cl clients)
            ))
            (catch Throwable e
              (println "Error waiting for client : " e)
              (close-server)
            )
          )
        )
        (while @active? (Thread/sleep 1))
        (doseq [c @clients]
          (when (-> c .isClosed not)
            (-> c .getOutputStream DataOutputStream. (.writeUTF "end"))
          )
        )
        (.close serv)
        (close-server)
        (println "server closed")
      )
      (catch Throwable e
        (println "Starting server error : " e)
        (close-server)
      )
    )
  )
  nil
)

(defn start-udp-listener [udp-port message]
  (let [
      socket (DatagramSocket. udp-port)
      task (future
        (while (running?)
          (try
            (let [
                buffer (byte-array 1024)
                packet (DatagramPacket. buffer (count buffer))
              ]
              (.receive socket packet)
              (let [
                  client-address (.getAddress packet)
                  client-port (.getPort packet)
                  response-bytes (.getBytes message)
                  response-packet (DatagramPacket. response-bytes (count response-bytes) client-address client-port)
                ]
                (.send socket response-packet)
                (println "Responded to" (.getHostAddress client-address) ":" client-port)
              )
            )
            (catch Throwable e (println "Error with UDP listener" e))
          )
        )
      )
    ]
    (println "UDP listener started on port" udp-port)
    (future
      (while (running?) (Thread/sleep 1))
      (future-cancel task)
      (.close socket)
      (println "udp listener stopped")
    )
  )
)
