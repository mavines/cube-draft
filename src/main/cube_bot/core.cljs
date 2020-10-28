(ns cube-bot.core
  (:require ["discord.js" :as Discord]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [cube-bot.config :as config]
            [cube-bot.cube :as cube]))

(defn jsprint [js]
  (.log js/console js))
(defonce prefix "[]")
(defonce *client (atom nil))
(defonce *debug-message (atom nil))

;; {:cube [current cards]
;;  :pack-number 1
;;  :remaining-packs [[cards in pack]]
;;  :seats [
;;    {:player {:id user-id :picks [card picks]
;;     :packs [[cards in pack] [cards in pack]]}}]} Packs are a Queue
(defonce *draft (atom nil))

(defn send-dm! [user-id text]
  (-> (.. ^js @*client -users -cache)
      (.get user-id)
      (.send text)))

(defn send-pack [seat]
  (let [user-id (get-in seat [:player :id])
        pack (first (:packs seat))
        pack-string (str/join "\n" (map #(str %1 " - " %2) (range (count pack)) pack))]
    (println pack-string)
    (send-dm! user-id pack-string)))

(defn build-seat [user-id pack]
  {:player {:id user-id :picks []}
   :packs [pack]})

(defn start-draft [user-ids]
  (println "Starting draft: " user-ids)
  (let [packs (partition 15 cube/combo)
        seats (map build-seat user-ids packs)]
    (reset! *draft {:pack-number 1
                    :remaining-packs (drop (count user-ids) packs)
                    :seats seats})
    (mapv send-pack (:seats @*draft))))

(defn pick-card [userId cardNumber]
  (println "Picking card: " userId " - " cardNumber))

(defn handle-command [^js message]
  (let [body (.-content message)
        command-string (subs body (count prefix))
        command-list (str/split command-string " ")
        command (first command-list)
        args (rest command-list)]
    (condp = command
      "newdraft" (start-draft (.. message -mentions -users keyArray))
      "pick" (pick-card (.. message -author -id) (first args)))))

;; Handle messages
(defn message-handler [^js message]
  (reset! *debug-message message)
  (let [body (.-content message)]
    (when (and (not (.. message -author -bot))
             (str/starts-with? body prefix))
      (handle-command message )))

  #_(if-not (= (.-author message) (.-user client))
    (cond
    ;; Mentioned
    ;; TODO: change the message when someone mentions the bot.
      (-> message (.isMentioned (.-user client)))
      (-> message
          (.-channel)
          (.send "Its a message for me"))
    ;; Specific words
    ;; TODO: add commands for the bot
      (-> message (.-content) (.startsWith "!bot"))
      (-> message
          (.-channel)
          (.send (str (-> message (.-author) (.toString))
                      ", hohoho who's the bot?!")))
    ;; Everything else
    ;; TODO: change what to do when users message between each other
      :else (-> message
                (.-channel)
                (.send "Just reading...")))))

(defn connect []
  (reset! *client (Discord/Client.))
  (.on ^js @*client "message" message-handler)
  (.login ^js @*client config/token))

(defn ^:dev/after-load reload! []
  (print "Code Reloaded")
  (.destroy ^js @*client)
  (connect))


(defn -main []
  (print "Starting bot...")
  (connect))
