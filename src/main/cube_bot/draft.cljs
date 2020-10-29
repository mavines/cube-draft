(ns cube-bot.draft
  (:require [clojure.string :as str]))

(defn pack->text [pack]
  (str/join "\n" (map #(str %1 " - " %2) (range (count pack)) pack)))

(defn build-seat [user-id pack]
  {:player {:id user-id :picks []}
   :packs [pack]})

(defn build-draft
  ([cube user-ids] (build-draft cube user-ids 15 3))
  ([cube user-ids pack-size num-packs]
   (let [player-count (count user-ids)
         packs (take (* num-packs player-count) (partition pack-size cube))
         seats (mapv build-seat user-ids packs)]
     {:pack-number 1
      :num-packs num-packs
      :remaining-packs (drop player-count packs)
      :seats seats})))

(defn players-seat [draft user-id]
  (first (filter #(= user-id (get-in % [:player :id]))
                 (:seats draft))))

(defn seat->player-id [seat]
  (get-in seat [:player :id]))

(defn player-id->seat-idx [user-id seats]
  (let [ids (mapv seat->player-id seats)]
    (.indexOf ids user-id)))

(defn next-seat [draft user-id]
  (let [seats (:seats draft)
        ids (mapv seat->player-id seats)
        seat-idx (.indexOf ids user-id)
        next-seat-idx (mod (inc seat-idx) (count ids))]
    (get seats next-seat-idx)))

(defn swap-seat [seats updated-seat]
  (let [user-id (seat->player-id updated-seat)
        seat-idx (player-id->seat-idx user-id seats)]
    (assoc-in seats [seat-idx] updated-seat)))

(defn send-next-pack? [draft picking-user]
  (let [picking-seat (players-seat draft picking-user)]
    (< 0 (count (:packs picking-seat)))))

(defn send-neighbor-pack? [draft picking-user]
  (let [next-seat (next-seat draft picking-user)]
    (= 1 (count (:packs next-seat)))))


(defn build-pack-message [seat]
  (let [user-id (get-in seat [:player :id])
        pack (first (:packs seat))
        pack-string (pack->text pack)]
    {:type :dm
     :user-id user-id
     :content pack-string}))

(defn pick-results [draft picking-user]
  (cond-> []
    (send-next-pack? draft picking-user) (conj (build-pack-message (players-seat draft picking-user)))
    (send-neighbor-pack? draft picking-user) (conj (build-pack-message (next-seat draft picking-user)))))

(defn perform-pick [draft user-id pick-number]
  (let [seat (players-seat draft user-id)
        player (:player seat)
        pack (first (:packs seat))
        pick (nth pack pick-number)
        picked-pack (remove #(= pick %) pack)
        next-seat (next-seat draft user-id)
        updated-seat (-> seat
                         (update :packs rest)
                         (update-in [:player :picks] conj pick))
        updated-next-seat (update next-seat :packs
                                  #(cond-> % (not-empty picked-pack) (conj picked-pack)))
        updated-draft (-> draft
                          (update :seats swap-seat updated-seat)
                          (update :seats swap-seat updated-next-seat))
        resulting-messages (pick-results updated-draft user-id)]
    {:draft updated-draft
     :messages resulting-messages}))
