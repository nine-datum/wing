(ns wing.server
  (:require [aleph.http :as http]
            [manifold.stream :as stream]))

(defn handler [request] (println request)
  {:status 200
   :body "Hello from server!"})

(defn start []
  (http/start-server handler {:port 8081}))
