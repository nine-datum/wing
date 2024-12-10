(ns wing.client
  [:import
    [java.io DataInputStream DataOutputStream BufferedInputStream]
    [java.net Socket DatagramSocket DatagramPacket InetAddress NetworkInterface]
  ]
)

(def active? (atom false))
(def rate 20)
(def sent-message (atom nil))
(def got-messages (atom nil))

(defn running? [] @active?)

(declare accept)

(defn send! [val]
  (reset! sent-message val)
)

(defn accept [name val]
  (swap! got-messages #(assoc % name val))
)

(defn got []
  @got-messages
)

(defn close-client []
  (reset! active? false)
)

(defn handle-in [sock]
  (future
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          last (atom "")
        ]
        (while (and @active? (-> sock .isClosed not) (not= @last "end"))
          (reset! last (.readUTF in))
          (let [
              l @last
              [name val] (when (not= l "end") (read-string l))
            ]
            (when name (accept name val))
          )
        )
      )
      (catch Throwable e
        (println "Handling input from server error : " e)
        (close-client)
      )
    )
  )
)

(defn start-client [addr port name]
  (reset! active? true)
  (future
    (try
      (let [
          sock (Socket. addr port)
          out (-> sock .getOutputStream DataOutputStream.)
        ]
        (println "client started")
        (handle-in sock)
        (while @active?
          (try (do
              (swap! sent-message #(when %
                  (.writeUTF out (pr-str (list name %)))
                )
              )
              (Thread/sleep (/ 1 rate 1/1000))
            )
            (catch Throwable e
              (println "Error sending client message" e)
              (close-client)
            )
          )
        )
        (.writeUTF out "end")
        (println "client closed")
        (.close out)
        (.close sock)
      )
      (catch Throwable e
        (println "Error starting client" e)
        (close-client)
      )
    )
  )
  nil
)

(defn get-local-addresses []
  (->>
    (NetworkInterface/getNetworkInterfaces)
    (enumeration-seq)
    (mapcat #(-> % .getInetAddresses enumeration-seq))
    (map #(.getHostAddress %))
    set
  )
)

(defn discover-servers [udp-port result-fn]
  (future
    (try
      (let [
            socket (DatagramSocket.)
            message-bytes (.getBytes "Are there servers?")
            broadcast-address (InetAddress/getByName "192.168.1.255")
            packet (DatagramPacket. message-bytes (count message-bytes) broadcast-address udp-port)]
        (.setBroadcast socket true)
        (.send socket packet)
        (println "Broadcast message sent")
        (let [buffer (byte-array 1024)
              response-packet (DatagramPacket. buffer (count buffer))]
          (try
            (.setSoTimeout socket 3000) ; Ждем максимум 3 секунды
            (.receive socket response-packet)
            (let [server-address (.getAddress response-packet)
                server-data (String. (.getData response-packet) 0 (.getLength response-packet))
                addr (.getHostAddress server-address)
                own-addrs (get-local-addresses)
                addr (if (contains? own-addrs addr) "localhost" addr)
              ]
              (println "Discovered server at" addr "with message:" server-data)
              (result-fn addr server-data)
            )
            (catch Exception e
              (println "No servers found")
            )
          )
        )
      )
      (catch Throwable e (println "Discowering servers error : " e))
    )
  )
  nil
)
