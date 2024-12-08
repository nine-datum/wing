(ns wing.client
  (:require [aleph.http :as http]
            [manifold.stream :as stream]))

(defn get-server-data []
  (let [response (http/get "http://localhost:8081")]
    (println response)))

(defn start []
  (get-server-data))
