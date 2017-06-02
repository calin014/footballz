(ns ^:figwheel-always footballz.client.main
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require [reagent.core :as r]
            [goog.events :as events]
            [goog.events.KeyCodes :as KeyCodes]
            [cljs.core.async :as async :refer (<! >! put! chan)]
            [taoensso.sente :as sente :refer (cb-success?)]
            [footballz.shared.game :as g]))

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" {:type :auto})]
  (def chsk chsk)
  (def ch-recv ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def chsk-state state))

(defonce state (r/atom {:joined? false
                        :game   (g/new-game)}))


;https://stackoverflow.com/questions/5203407/javascript-multiple-keys-pressed-at-once
;TODO: send keyup and keydown and let server decide, in order to emulate accel
;TODO: move state update in cljc physics ns and use it on client for guessing stuff

(defn command-sender [type]
  (fn [e]
    (when (:joined? @state)
      (condp = (.-keyCode e)
        KeyCodes/LEFT (chsk-send! [:footballz/command [type :decel-x]])
        KeyCodes/RIGHT (chsk-send! [:footballz/command [type :accel-x]])
        KeyCodes/UP (chsk-send! [:footballz/command [type :decel-y]])
        KeyCodes/DOWN (chsk-send! [:footballz/command [type :accel-y]])
        nil))))


(events/listen js/document "keydown" (command-sender :add))
(events/listen js/document "keyup" (command-sender :remove))


(defmulti event-handler :id)

(defmethod event-handler :default [{:as ev-msg :keys [event]}]
  (println "Unhandled event: %s" event))

(defmethod event-handler :chsk/recv [{:as ev-msg :keys [?data]}]
  (swap! state assoc :game (second ?data)))

(sente/start-client-chsk-router! ch-recv event-handler)

(defn atom-input [value]
  [:input {:type      "text"
           :value     @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn join-game []
  (let [name (r/atom "")
        join (fn [e]
               (chsk-send! [:footballz/join {:name @name}])
               (swap! state assoc :joined? true))]
    (fn []
      [:div
       [atom-input name]
       [:input {:type     "button"
                :value    "Join"
                :on-click join}]])))

(defn footballz-field [world]
  [:svg {:width g/field-width :height g/field-height}
   [:rect {:width "100%" :height "100%" :fill "green"}]
   (for [[pid {:keys [name radius color]
               [x y] :position}] (:entities world)]
     ^{:key pid} [:g
                  [:circle {:cx           x
                            :cy           y
                            :r            radius
                            :fill         color
                            :stroke-width 0.2
                            :stroke       "green"}]
                  [:text {:fill      "black"
                          :font-size 17
                          :x         (- x 15)
                          :y         y} name]])])


(defn footballz-game []
  [:div
   [:h1 "Football-z"]
   (if (not (:joined? @state)) [join-game])
   [footballz-field (:game @state)]])

(r/render-component
  [footballz-game]
  (. js/document (getElementById "app")))