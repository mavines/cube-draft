(ns cube-bot.core
  (:require ["discord.js" :as Discord]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [cube-bot.config :as config]
            [cube-bot.cube :as cube]
            [cube-bot.draft :as draft]
            [cube-bot.util :as util :refer [jsprint drop-nth]]))

(defonce prefix "[]")
(defonce *client (atom nil))
(defonce *debug-message (atom nil))

;; {:pack-number 1
;;  :remaining-packs [[cards in pack]]
;;  :seats [
;;    {:player {:id user-id :picks [card picks]
;;     :packs [[cards in pack] [cards in pack]]}}]} Packs are a Queue
(defonce *draft (atom nil))

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
   (println "Starting draft: " user-ids)
   (let [draft (draft/build-draft cube/combo user-ids num-packs pack-size)]
     (reset! *draft draft)
     (mapv send-pack! (:seats @*draft)))))

(defn handle-pick! [user-id pick-number]
  (let [{:keys [draft messages]} (draft/perform-pick @*draft user-id pick-number)]
    (reset! *draft draft)
    (send! messages)))

;; Valids args are a cube ID
;; Optional number of packs and pack size
(defn sanitize-start-draft-inputs [args]
  (->> args
       (map js/parseInt)
       (remove js/Number.isNaN)
       (remove #(> 0 %))
       (take 3)))

(defn send-error! [^js message]
  )

(defn handle-command! [^js message]
  (let [body (.-content message)
        command-string (subs body (count prefix))
        command-list (str/split command-string " ")
        command (first command-list)
        args (rest command-list)]
    (condp = command
      "newdraft" (if-let [draft-args (sanitize-start-draft-inputs args)]
                   (apply start-draft! (.. message -mentions -users keyArray) draft-args)
                   (print "send-start-help!"))
      "pick" (handle-pick! (.. message -author -id) (js/parseInt (first args))))))

;; Handle messages
(defn message-handler [^js message]
  (try
    (let [body (.-content message)]
      (when (and (not (.. message -author -bot))
                 (str/starts-with? body prefix))
        (handle-command! message)))
    (catch js/Error e
      (println "Error occurred: " e)
      (send-error! message)))

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


;; TODO - to use locally:
;; - Check all inputs so it doesn't crash
;; - Allow 'newdraft' arguments to work
;; - Add '[]picks' command
;; - Add '[]help'

;; TODO - Big Picture
;; - Import cube tutor / cube cobra
;; - Different drafts (id?)
;; - Persistence
;; - Hosting
