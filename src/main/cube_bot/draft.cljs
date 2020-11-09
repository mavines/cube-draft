(ns cube-bot.draft
  (:require [cube-bot.polyfills]
            [cube-bot.macros :refer [error-let]]
            [clojure.string :as str]
            [nano-id.core :refer [nano-id]]))


(defn pack->text [pack]
  (str/join "\n" (map #(str %1 " - " %2) (range (count pack)) pack)))

(defn build-seat [user-id pack]
  {:player {:id user-id :picks []}
   :packs [pack]})

(defn build-draft
  ([cube user-ids] (build-draft cube user-ids 3 15))
  ([cube user-ids num-packs pack-size]
   (let [player-count (count user-ids)
         packs (take (* num-packs player-count) (partition pack-size cube))
         starting-packs (take player-count packs)
         seats (mapv build-seat user-ids packs)]
     {:draft-id (nano-id 6)
      :pack-number 1
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
        player-count (count ids)
        next-seat-idx (if (odd? (:pack-number draft))
                        (mod (inc seat-idx) player-count)
                        (mod (+ player-count (dec seat-idx)) player-count))]
    (get seats next-seat-idx)))

(defn swap-seat [seats updated-seat]
  (let [user-id (seat->player-id updated-seat)
        seat-idx (player-id->seat-idx user-id seats)]
    (assoc-in seats [seat-idx] updated-seat)))

(defn send-next-pack? [draft picking-user]
  (let [picking-seat (players-seat draft picking-user)]
    (< 0 (count (:packs picking-seat)))))

(defn send-neighbor-pack? [draft picking-user]
  (let [picking-seat (players-seat draft picking-user)
        next-seat (next-seat draft picking-user)]
    ;; Neighbor has a pack and we have a pack
    ;; means we just passed the pack.
    (and (= 1 (-> next-seat :packs count))
         (<= 1 (-> picking-seat :packs count)))))


(defn build-pack-message [draft-id seat]
  (let [user-id (get-in seat [:player :id])
        pack (first (:packs seat))
        pack-string (pack->text pack)]
    {:type :dm
     :user-id user-id
     :content (str "Make a pick for Draft: " draft-id "\n" pack-string
                   "\n\n To Pick:  []pick " draft-id " n ")}))

(defn packs-empty? [draft]
  (->> draft
       :seats
       (map :packs)
       (remove empty?)
       empty?))

(defn draft-done? [draft]
  (and (packs-empty? draft)
       (= (:pack-number draft) (:num-packs draft))))

(defn end-draft-message [seat]
  {:type :dm
   :user-id (-> seat :player :id)
   :content (str "The draft has ended!\nYour picks are:\n"
                 (str/join "\n" (-> seat :player :picks)))})

(defn end-draft-messages [draft]
  (mapv end-draft-message (:seats draft)))

(defn pick-results [draft picking-user]
  (cond-> []
    (send-next-pack? draft picking-user) (conj (build-pack-message (:draft-id draft)
                                                                   (players-seat draft picking-user)))
    (send-neighbor-pack? draft picking-user) (conj (build-pack-message (:draft-id draft)
                                                                       (next-seat draft picking-user)))
    (draft-done? draft) (#(apply conj % (end-draft-messages draft)))))

(defn set-seat-pack [seat pack]
  (update seat :packs conj pack))

(defn next-pack [draft]
  (if (= (:num-packs draft)
         (:pack-number draft))
    draft
    (let [seats (:seats draft)
          packs (take (count seats) (:remaining-packs draft))]
      (-> draft
          (update :pack-number inc)
          (update :seats #(mapv set-seat-pack % packs))
          (update :remaining-packs #(drop (count seats) %))))))

(defn player-picks [draft user-id]
  (-> (players-seat draft user-id)
      :player
      :picks))

(defn perform-pick [draft user-id pick-number]
  (error-let [seat (players-seat draft user-id) {:error "Player not found in draft."}
              pack (first (:packs seat)) {:error "No pack found for player"}
              player (:player seat) {:error"Player not found in seat"}
              valid-pick (< pick-number (count pack)) {:error "Invalid card selected from pack"}
              pick (nth pack pick-number) {:error "Invalid card selected from pack"}
              picked-pack (remove #(= pick %) pack) {:error "Failed picking a card"}
              next-seat (next-seat draft user-id) {:error "Cannot find the next player"}
              updated-seat (-> seat
                               (update :packs subvec 1)
                               (update-in [:player :picks] conj pick)) {:error "Unable to update the seat"}
              updated-next-seat (update next-seat :packs
                                        #(cond-> % (not-empty picked-pack)
                                                 (conj picked-pack))) {:error "Unable to update next seat"}
              updated-draft (-> draft
                                (update :seats swap-seat updated-seat)
                                (update :seats swap-seat updated-next-seat)
                                (#(if (packs-empty? %) (next-pack %) %))) {:error "Unable to update the draft"}
              resulting-messages (pick-results updated-draft user-id) {:error "Error getting messages"}]
             {:draft updated-draft
              :messages resulting-messages}))
