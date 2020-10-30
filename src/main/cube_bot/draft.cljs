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
         starting-packs (take player-count packs)
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
  (let [next-seat (next-seat draft picking-user)]
    ;; Has one pack and it has more than 1 card means we
    ;; just passed that pack
    (and (= 1 (-> next-seat :packs count))
         (< 1 (-> next-seat :packs first count)))))


(defn build-pack-message [seat]
  (let [user-id (get-in seat [:player :id])
        pack (first (:packs seat))
        pack-string (pack->text pack)]
    {:type :dm
     :user-id user-id
     :content pack-string}))

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
   :content "The draft has ended!\nRespond with '[]picks' to view your picks."})

(defn end-draft-messages [draft]
  (map end-draft-message (:seats draft)))

(defn pick-results [draft picking-user]
  (cond-> []
    (send-next-pack? draft picking-user) (conj (build-pack-message (players-seat draft picking-user)))
    (send-neighbor-pack? draft picking-user) (conj (build-pack-message (next-seat draft picking-user)))
    (draft-done? draft) (concat (end-draft-messages draft))))

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
          (update :seats #(mapv set-seat-pack % packs))))))

(defn perform-pick [draft user-id pick-number]
  (let [seat (players-seat draft user-id)
        player (:player seat)
        pack (first (:packs seat))
        pick (nth pack pick-number)
        picked-pack (remove #(= pick %) pack)
        next-seat (next-seat draft user-id)
        updated-seat (-> seat
                         (update :packs subvec 1)
                         (update-in [:player :picks] conj pick))
        updated-next-seat (update next-seat :packs
                                  #(cond-> % (not-empty picked-pack) (conj picked-pack)))
        updated-draft (-> draft
                          (update :seats swap-seat updated-seat)
                          (update :seats swap-seat updated-next-seat)
                          (#(if (packs-empty? %) (next-pack %) %)))
        resulting-messages (pick-results updated-draft user-id)]
    {:draft updated-draft
     :messages resulting-messages}))
