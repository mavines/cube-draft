(ns cube-bot.core
  (:require ["discord.js" :as Discord]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [cube-bot.config :as config]
            [cube-bot.cube :as cube]))

(defn jsprint [js]
  (.log js/console js))
(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

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
        seats (mapv build-seat user-ids packs)]
    (reset! *draft {:pack-number 1
                    :remaining-packs (drop (count user-ids) packs)
                    :seats seats})
    (mapv send-pack (:seats @*draft))))

(defn players-seat [user-id]
  (first (filter #(= user-id (get-in % [:player :id]))
                 (:seats @*draft))))

(defn seat->player-id [seat]
  (get-in seat [:player :id]))

(defn player-id->seat-idx [user-id seats]
  (let [ids (mapv seat->player-id seats)]
    (.indexOf ids user-id)))

(defn next-seat [user-id]
  (let [seats (:seats @*draft)
        ids (mapv seat->player-id seats)
        seat-idx (.indexOf ids user-id)
        next-seat-idx (if (= seat-idx (dec (count ids)))
                        0
                        (inc seat-idx))]
    (get seats next-seat-idx)))

(defn swap-seat [seats updated-seat]
  (let [user-id (seat->player-id updated-seat)
        seat-idx (player-id->seat-idx user-id seats)]
    (assoc-in seats [seat-idx] updated-seat)))

(defn pick-card [user-id pick-number]
  (let [seat (players-seat user-id)
        player (:player seat)
        pack (first (:packs seat))
        pick (nth pack pick-number)
        picked-pack (remove #(= pick %) pack)
        next-seat (next-seat user-id)
        updated-seat (-> seat
                         (update :packs rest)
                         (update :picks conj pick))
        updated-next-seat (update next-seat :packs conj picked-pack)
        updated-draft (-> @*draft
                          (update :seats swap-seat updated-seat)
                          (update :seats swap-seat updated-next-seat))]
    (reset! *draft updated-draft)))

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
