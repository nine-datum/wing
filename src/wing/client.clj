(ns wing.client
  [:import
    [java.io DataInputStream DataOutputStream BufferedInputStream]
    [java.net Socket DatagramSocket DatagramPacket InetAddress NetworkInterface]
    [java.nio ByteBuffer]
    [java.nio.charset Charset]
  ]
)

(def active? (atom false))
(def rate 20)
(def sent-message (atom nil))
(def got-messages (atom nil))
(def prev-messages (atom nil))
(def prev-time (atom nil))
(def client-uid (str (java.util.UUID/randomUUID)))
(def got-archive (atom nil))

(defn get-time [] (/ (System/currentTimeMillis) 1000))

(defn running? [] @active?)

(defn send! [val]
  (reset! sent-message val)
)

(defn accept [msg]
  (let [
      { :keys [name uid time val] } msg
    ]
    (when (and
        (not= uid client-uid)
        (-> @got-archive (get uid) (get uid) (get :time 0) (< time))
      )
      (reset! prev-messages @got-messages)
      (swap! got-messages #(assoc % uid val))
      (reset! prev-time (get-time))
      (swap! got-archive #(assoc % uid msg))
    )
  )
)

(defn got []
  @got-messages
)

(defn archive []
  @got-archive
)

(defn got-lerp [lerp-fn]
  (let [
      as @prev-messages
      bs @got-messages
      t @prev-time
      p (when t (-> (get-time) (- t) (* rate)))
    ]
    (when (and as bs p)
      (->> bs keys
        (map
          #(let [
              a (as %)
              b (bs %)
            ]
            (vector % (cond
              (and a b (= (count a) (count b))) (mapv lerp-fn a b (repeat p))
              :else b
            ))
          )
        )
        (into {})
      )
    )
  )
)

(defn close-client []
  (reset! active? false)
)

(defn clean []
  (reset! sent-message nil)
  (reset! got-messages nil)
  (reset! prev-messages nil)
  (reset! prev-time nil)
)

(def charset (Charset/forName "UTF-8"))

(defn bytes->string [bytes]
  (let [
      ^ByteBuffer bb (ByteBuffer/wrap bytes)
      len (.getInt bb)
      ^"[B" buf (byte-array len)
    ]
    (.get bb buf)
    (String. buf charset)
  )
)

(defn string->bytes [str]
  (let [
      len (.length str)
      ^"[B" buf (.getBytes str charset)
      ^"[B" res (-> buf alength (+ 4) byte-array)
      ^ByteBuffer bb (ByteBuffer/wrap res)
    ]
    (.putInt bb len)
    (.put bb buf)
    res
  )
)

(defn read-udp [port continue? func]
  (future
    (try
      (let [
          sock (DatagramSocket. port)
          buf (byte-array 2048)
          pack (DatagramPacket. buf (alength buf))
        ]
        (while (continue?)
          (.receive sock pack)
          (-> pack .getData bytes->string func)
          (Thread/sleep 1)
        )
        (.close sock)
      )
      (catch Throwable e (println "Reading UDP error : " e))
    )
  )
)

(defn send-udp [addr port continue? data-func]
  (future
    (try
      (let [
          sock (DatagramSocket.)
        ]
        (while (continue?)
          (let [
              dat (data-func)
              addr (InetAddress/getByName addr)
            ]
            (when dat
              (.send sock
                (DatagramPacket. dat (alength dat) addr port)
              )
            )
          )
          (Thread/sleep (/ 1 rate 1/1000))
        )
        (.close sock)
      )
      (catch Throwable e (println "Sending UDP error : " e))
    )
  )
)

(defn handle-in [sock]
  (future
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
          last (atom "")
        ]
        (println "Reading messages from server...")
        (while (and @active? (-> sock .isClosed not) (not= @last "end"))
          (->> in .readUTF (reset! last))
        )
        (println "Finished reading messages from server, last message was " @last)
        (close-client)
      )
      (catch Throwable e
        (println "Handling input from server error : " e)
        (close-client)
      )
    )
  )
)

(defn start-client [addr port udp-port name]
  (clean)
  (reset! active? true)
  (future
    (try
      (let [
          uid client-uid
          sock (Socket. addr port)
          out (-> sock .getOutputStream DataOutputStream.)
        ]
        (println "client started")
        (read-udp udp-port running?
          (fn [l]
            (let [
                map (when (-> l first (= (char 123))) (read-string l))
              ]
              (when map (doseq [p map] (-> p second accept)))
            )
          )
        )
        (handle-in sock)
        (send-udp addr udp-port running?
          #(let [msg @sent-message] (when msg (->> msg
            (hash-map :name name :time (get-time) :uid uid :val)
            (hash-map uid)
            pr-str string->bytes
          )))
        )
        (while (running?) (Thread/sleep 1))
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
            broadcast-address (InetAddress/getByName "255.255.255.255")
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
