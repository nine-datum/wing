(ns wing.client
  [:import
    [java.io DataInputStream DataOutputStream BufferedInputStream]
    [java.net Socket]
  ]
)

(def active? (atom false))
(def client-sock (atom nil))
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
  (swap! client-sock #(.close %))
)

(defn handle-in [sock]
  (future
    (try
      (let [
          in (-> sock .getInputStream BufferedInputStream. DataInputStream.)
        ]
        (while @active?
          (let [
              name (.readUTF in)
              msg (.readUTF in)
            ]
            (accept name (read-string msg))
          )
        )
      )
      (catch Throwable e (println "Handling input from server error : " e))
    )
  )
)

(defn start-client [addr port name]
  (future
    (try
      (let [
          sock (Socket. addr port)
          out (-> sock .getOutputStream DataOutputStream.)
        ]
        (reset! client-sock sock)
        (reset! active? true)
        (println "client started")
        (handle-in sock)
        (while @active?
          (swap! sent-message #(when (-> % nil? not)
              (.writeUTF out name)
              (.writeUTF out (pr-str %))
            )
          )
          (Thread/sleep (/ 1 rate 1/1000))
        )
        (.writeUTF out "end")
        (println "client closed")
        (.close out)
        (close-client)
      )
      (catch Throwable e (println e))
    )
  )
)
