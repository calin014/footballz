(ns footballz.server.main
  (:require
    [ring.middleware.defaults :as defaults]
    [ring.middleware.reload :as reload]
    [ring.middleware.cors :as cors]
    [ring.util.response :as response]
    [taoensso.sente :as sente]
    [org.httpkit.server :as server]
    [taoensso.sente.server-adapters.http-kit :refer (get-sch-adapter)]
    [compojure.core :refer [defroutes GET POST]]
    [compojure.route :as route]
    [environ.core :as environ]
    [footballz.shared.game :as g]))

(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket!
        (get-sch-adapter)
        {:user-id-fn (fn [ring-req] (:client-id ring-req))})]

  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-recv ch-recv)
  (def chsk-send! send-fn)
  (def connected-uids connected-uids))

(defonce game (ref (g/new-game)))

(defroutes routes
           (GET "/" req (response/content-type
                          (response/resource-response "public/index.html")
                          "text/html"))
           (GET "/status" req (str "Running: " (pr-str @connected-uids)))
           (GET "/chsk" req (ring-ajax-get-or-ws-handshake req))
           (POST "/chsk" req (ring-ajax-post req))
           (route/resources "/")
           (route/not-found "Not found"))

(def handler
  (-> #'routes
      (cond-> (environ/env :dev?) (reload/wrap-reload))
      (defaults/wrap-defaults (assoc-in defaults/site-defaults [:security :anti-forgery] false))
      (cors/wrap-cors :access-control-allow-origin [#".*"]
                      :access-control-allow-methods [:get :put :post :delete]
                      :access-control-allow-credentials ["true"])))


(defmulti event-handler :id)

(defmethod event-handler :default [{:as ev-msg :keys [event]}]
  (println "Unhandled event: %s" event))

(defmethod event-handler :chsk/uidport-open [{:keys [uid client-id]}]
  (println "New connection:" uid client-id))

(defmethod event-handler :chsk/ws-ping [_])

(defmethod event-handler :chsk/uidport-close [{:keys [uid]}]
  (dosync (alter game g/leave uid))
  (println "Disconnected:" uid))

(defmethod event-handler :footballz/join [{:as ev-msg :keys [event uid ?data]}]
  (dosync (alter game g/join uid ?data)))

(defmethod event-handler :footballz/leave [{:as ev-msg :keys [event uid]}]
  (dosync (alter game g/leave uid)))

(defmethod event-handler :footballz/command [{:as ev-msg :keys [event uid ?data]}]
  (dosync (alter game g/register-player-command uid ?data)))

(sente/start-chsk-router! ch-recv event-handler)

(defn broadcast []
  (doseq [uid (:any @connected-uids)]
    (chsk-send! uid [:footballz/world @game])))

(def delta 50)
(defn ticker []
  (while true
    (Thread/sleep delta)
    (try
      (dosync (alter game g/update-game delta))
      (broadcast)
      (catch Exception ex
        (println ex)))))

(defn start-ticker []
  (defonce ticker-thread
           (doto (Thread. ticker)
             (.start))))

(defn -main [& args]
  (println "Server starting...")
  (start-ticker)
  (server/run-server #'handler
                     {:port (or (some-> (first args) (Integer/parseInt))
                                (environ/env :http-port 3000))}))