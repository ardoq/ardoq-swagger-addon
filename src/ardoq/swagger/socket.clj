(ns ardoq.swagger.socket
  (:require [org.httpkit.server :as srv]))


(def ch (atom nil))

(defn handler [system request]
  (srv/with-channel request channel
    (srv/on-close channel (fn [status]
                            (reset! ch nil)
                            (println "Channel closed")))
    (if (srv/websocket? channel)
      (println "WebSocket channel")
      (println "HTTP channel"))
    (reset! ch channel)))

(defn socket-send [msg]
  (when @ch
    (srv/send! @ch msg false)))

(defn socket-close []
  (when @ch
    (srv/close @ch)))
