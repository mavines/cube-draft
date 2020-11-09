(ns cube-bot.core
  (:require ["discord.js" :as Discord]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [cljs.core.async :as a :refer [<! >! go put! take! chan to-chan!]]
            [cube-bot.cobra :as cobra]
            [cube-bot.config :as config]
            [cube-bot.cube :as cube]
            [cube-bot.db :as db]
            [cube-bot.draft :as draft]
            [cube-bot.macros :refer [error-let]]
            [cube-bot.util :as util :refer [jsprint drop-nth]]
            [cube-bot.error :as e]
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
  (println message)
  (condp = (:type message)
    :dm (send-dm! message)))

(defn send! [messages]
  (if (vector? messages)
    (mapv send-message! messages)
    (send-message! messages)))

(defn error-message [user-id]
  {:type :dm
   :user-id user-id
   :content "An error occurred with your message, please try again."})

(defn handle-error [text user-id]
  (error text)
  {:type :dm
   :user-id user-id
   :content text})

(defn send-error! [user-id]
  (send-message! (error-message user-id)))

(defn help-message [user-id]
  {:type :dm
   :user-id user-id
   :content
   "Commands:
'[]pick id n' - Pick card 'n' from your current pack in draft 'id'.
'[]picks id' - Show your current picks in draft 'id'.
'[]newdraft @xxx @yyy 'cube-id' 'packs' 'packsize' - Starts a new draft with the mentioned players, the CubeCobra cube matching the id, the given number of packs of the given size."})

(defn send-help! [user-id]
  (send-message! (help-message user-id)))

(defn start-draft!
  ([user-ids cube-id] (start-draft! user-ids cube-id 3 15))
  ([user-ids cube-id num-packs] (start-draft! user-ids cube-id num-packs 15))
  ([user-ids cube-id num-packs pack-size]
   (info "Starting draft: CubeID " cube-id " players: " user-ids)
   (go (let [cube (<! (cobra/get-cube cube-id))]
         (if-let [err (:error cube)]
           (handle-error (str "Error getting cube: " cube-id) (first user-ids))
           (let [draft (draft/build-draft (shuffle cube) user-ids num-packs pack-size)
                 save-status (db/save-draft draft)]
             (if-let [err (:error save-status)]
               (handle-error err (first user-ids))
               (mapv #(draft/build-pack-message (:draft-id draft) %)
                     (:seats draft)))))))))

(defn handle-pick! [user-id draft-id pick-number]
  (if (js/Number.isNaN pick-number)
    (handle-error "Must enter a card to pick" user-id)
    (go (let [saved-draft (<! (db/get-draft draft-id))]
          (if (:error saved-draft)
            (error-message user-id)
            (let [pick-result (draft/perform-pick saved-draft user-id pick-number)]
              (if-let [err (:error pick-result)]
                (handle-error err user-id)
                (let [update-status (<! (db/update-draft (:draft pick-result)))]
                  (if-let [err (:error update-status)]
                    (handle-error err user-id)
                    (:messages pick-result))))))))))

(defn show-picks-message [draft user-id]
  (let [picks (draft/player-picks draft user-id)]
    {:type :dm
     :user-id user-id
     :content (str "Picks for draft: " (:draft-id draft) "\n" (draft/pack->text picks))}))

(defn handle-show-picks [user-id draft-id]
  (go (let [draft (<! (db/get-draft draft-id))]
        (if-let [err (:error draft)]
          (handle-error err user-id)
          (show-picks-message draft user-id)))))

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
                   (to-chan! [(help-message author-id)]))
      "pick" (handle-pick! author-id (first args) (js/parseInt (second args)))
      "picks" (handle-show-picks author-id (first args))
      "help" (to-chan! [(help-message author-id)])
      (to-chan! [(help-message author-id)]))))

;; Handle messages
(defn message-handler [^js message]
  (try
    (let [body (.-content message)]
      (when (and (not (.. message -author -bot))
                 (str/starts-with? body prefix))
        (let [result (handle-command! message)]
          (take! result #(do (info "Result:" %)
                             (send! %))))))
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


;; TODO
;; - Check all inputs so it doesn't crash
