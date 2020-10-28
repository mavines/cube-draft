(ns cube-bot.core
  (:require ["discord.js" :as Discord]
            [clojure.string :as str]
            [cube-bot.config :as config]))


(defonce *client (atom nil))

(defonce prefix "[]")

(defn handle-command [command]
  (println command))

;; Handle messages
(defn message-handler [^js message]
  (let [body (.-content message)]
    (when (and (not (.. message -author -bot))
             (str/starts-with? body prefix))
      (handle-command (subs body (count prefix)))))

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
  (js/console.log "Code Reloaded")
  (.destroy ^js @*client)
  (connect))


(defn -main []
  (print "MAIN")
  (connect))
