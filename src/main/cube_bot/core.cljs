(ns cube-bot.core
  (:require ["discord.js" :as Discord]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [cube-bot.cobra :as cobra]
            [cube-bot.config :as config]
            [cube-bot.cube :as cube]
            [cube-bot.db :as db]
            [cube-bot.draft :as draft]
            [cube-bot.util :as util :refer [jsprint drop-nth]]
            [taoensso.timbre.appenders.community.node-spit :as spitter]
            [taoensso.timbre :as timbre
             :refer-macros [log trace debug info warn error fatal report spy get-env]]))

(defonce prefix "[]")
(defonce *client (atom nil))
(defonce *debug-message (atom nil))

(defn send-dm! [message]
  (let [{:keys [user-id content]} message]
    (-> (.. ^js @*client -users -cache)
        (.get user-id)
        (.send content))))

(defn send-message! [message]
  (condp = (:type message)
    :dm (send-dm! message)))

(defn send! [messages]
  (mapv send-message! messages))

(defn send-pack! [seat]
  (let [user-id (get-in seat [:player :id])
        pack (first (:packs seat))
        pack-string (draft/pack->text pack)
        message {:type :dm
                 :user-id user-id
                 :content pack-string}]
    (send-message! message)))

(defn start-draft!
  ([user-ids cube-id] (start-draft! user-ids cube-id 3 15))
  ([user-ids cube-id num-packs] (start-draft! user-ids cube-id num-packs 15))
  ([user-ids cube-id num-packs pack-size]
   (info "Starting draft: CubeID " cube-id " players: " user-ids)
   (cobra/get-cube cube-id
                   (fn [cube-list]
                     (let [draft (draft/build-draft (shuffle cube-list) user-ids num-packs pack-size)]
                       (db/save-draft draft)
                       (mapv send-pack! (:seats draft)))))))

(defn handle-pick! [user-id draft-id pick-number]
  (db/get-draft draft-id
                (fn [err draft]
                  (try
                    (if err
                      (error "Error getting draft: " err)
                      (let [{:keys [draft messages]} (draft/perform-pick draft user-id pick-number)]
                        (db/update-draft draft)
                        (send! messages)))
                    (catch js/Error e
                      (error "Error occured picking: " e))))))

(defn show-picks-message [draft user-id]
  (let [picks (draft/player-picks draft user-id)]
    {:type :dm
     :user-id user-id
     :content (draft/pack->text picks)}))

(defn handle-show-picks! [user-id draft-id]
  (db/get-draft draft-id
                (fn [draft]
                  (send-message! (show-picks-message draft user-id)))))

;; Valids args are a cube ID
;; Optional number of packs and pack size
(defn sanitize-start-draft-inputs [args]
  (let [cube-id (first args)]
    (->> (rest args)
         (map js/parseInt)
         (remove js/Number.isNaN)
         (remove #(> 0 %))
         (take 2)
         (#(conj % cube-id)))))

(defn send-error! [user-id]
  (send-message! {:type :dm
                  :user-id user-id
                  :content "An error occurred with your message, please try again."}))

(defn send-help! [user-id]
  (send-message! {:type :dm
                  :user-id user-id
                  :content
                  "Commands:
'[]pick id n' - Pick card 'n' from your current pack in draft 'id'.
'[]picks id' - Show your current picks in draft 'id'."}))

(defn handle-command! [^js message]
  (let [body (.-content message)
        command-string (subs body (count prefix))
        command-list (str/split command-string " ")
        command (first command-list)
        player-ids (.. message -mentions -users keyArray)
        args (drop (inc (count player-ids)) command-list)
        author-id (.. message -author -id)]
    (debug "Command: " command " Args: " args)
    (condp = command
      "newdraft" (if-let [draft-args (sanitize-start-draft-inputs args)]
                   (apply start-draft! player-ids draft-args)
                   (send-help! author-id))
      "pick" (handle-pick! author-id (first args) (js/parseInt (second args)))
      "picks" (handle-show-picks! (first args) author-id)
      "help" (send-help! author-id)
      (send-help! author-id))))

;; Handle messages
(defn message-handler [^js message]
  (try
    (let [body (.-content message)]
      (when (and (not (.. message -author -bot))
                 (str/starts-with? body prefix))
        (handle-command! message)))
    (catch js/Error e
      (error "Error occurred: " e)
      (send-error! (.. message -author -id))))

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
  (.login ^js @*client config/token)
  (db/connect db/url))

(defn ^:dev/after-load reload! []
  (debug "Code Reloaded")
  (.destroy ^js @*client)
  (db/disconnect)
  (connect))

(defn configure-logging [log-level]
  (timbre/set-level! log-level)
  (timbre/merge-config! {:appenders
                         {:spit (spitter/node-spit-appender
                                 {:fname "log.txt"
                                  :append? true})}}))

(defn -main []
  (configure-logging :debug)
  (info "Starting bot...")
  (connect))


;; TODO - to use locally:
;; - Check all inputs so it doesn't crash

;; TODO - Big Picture
;; - Import cube tutor / cube cobra
;; - Different drafts (id?)
;; - Persistence
;; - Hosting
