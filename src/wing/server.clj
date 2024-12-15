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

(defn handle-client [sock clients udp-port]
  (future
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          addr (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          client-active? (atom true)
          last (atom "")
        ]
        (println "client connected : " name ", clients total : " (count @clients))
        (client/send-udp (-> sock .getInetAddress .getHostAddress) udp-port running?
          (comp client/string->bytes pr-str (partial apply merge) client/archive)
        )
        (while (and active? (not= @last "end"))
          (->> in .readUTF (reset! last))
        )
        (println "client disconnected :" name ", last message was :" @last)
        (.close in)
        (.close sock)
        (swap! clients #(disj % sock))
      )
      (catch Throwable e (println "Handling client error : " e))
    )
  )
  nil
)

(declare start-udp-listener)

(defn start-server [port udp-port broadcast-port name]
  (reset! active? true)
  (future
    (try
      (let [
          serv (ServerSocket. port)
          clients (atom (hash-set))
        ]
        (start-udp-listener broadcast-port name)
        (println "server started")
        (future
          (try
            (while active? (let [
                new-cl (.accept serv)
              ]
              (swap! clients #(conj % new-cl))
              (handle-client new-cl clients udp-port)
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
