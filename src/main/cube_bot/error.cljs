(ns cube-bot.error
  (:require [cljs.core :as core]
            [cljs.core.async :as a :refer [<! >! go put! take! chan]]
            [cube-bot.draft :as draft :refer [players-seat next-seat pick-results swap-seat next-pack packs-empty?]]
            [cube-bot.macros :as macros :refer-macros [else-let error-let]]))

(defn perform-pick [draft user-id pick-number]
  (let [seat (players-seat draft user-id)]
    (if-let [pack (first (:packs seat))]
      (if (>= pick-number (count pack))
        {:draft draft
         :messages [{:type :dm
                     :user-id user-id
                     :content "Not a valid card number."}]}
        (let [player (:player seat)
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
      {:draft draft
       :messages [{:type :dm
                   :user-id user-id
                   :content "No Pick to make"}]})))


(defn apply-or-error [f [val err]]
  (if err
    [val err]
    (f val)))


(defmacro =>>
  "Threading macro, will execute a given set of functions one by one.
  If one of the functions fails, it will skip the rest of the functions and will return a fail cause."
  [val & funcs]
  (let [steps (for [f funcs] `(apply-or-error ~f))]
    `(->> [~val nil]
          ~@steps)))


(else-let [a (even? 4)]
          (println "odd")
          (println "even"))


(defn pick [draft]
  (else-let [seat (:seat draft)] {:error "No seat found for player"}
            (else-let [player (:player seat)] {:error "No Player found"}
                      "Found everything")))


(pick {:seat {:player "mason"}
       :pack "pack"})

(defn error-let-ex [draft]
  (let [seat (:seat draft)]
    (if seat
      (let [player (:player seat)]
        (if player
          "Found everything"
          {:error "no player"}))
      {:error "no seat"})))

#_(defn pick-single [draft]
  (error-let [seat (:seat draft) {:error "No seat found for player"}]
             "Found everything"))

#_(pick-single {:seat {:player nil}
              :pack "pack"})

(defn pick-error [draft]
  (error-let [seat (:seat draft) {:error "No seat found for player"}
              player (:player seat) {:error "No Player found"}
              thing 5 {:error "thing nil"}]
             thing))

(defn single-error [draft]
  (error-let [seat (:seat draft) {:error "No seta"}]
             (:player seat)))

(pick-error {:seat {:player "mason"}})

(single-error {:seat {:player "Mason"}})

(def nums [1 2 3 4 5 6 7 8 9])


(reduce (fn [inner cur]
          (let [form (first cur)
                tst (second cur)
                err (nth cur 2)]
            (str "(let [temp# ~tst]
(if temp#
(let [~form temp#]
~inner)
~err))")))
        "All passed"
        (reverse (partition 3 nums)))


(defn do-pick [draft user-id pick-number]
  (let [seat (players-seat draft user-id)
        pack (first (:packs seat))
        player (:player seat)
        pick (nth pack pick-number)
        picked-pack (remove #(= pick %) pack)
        next-seat (next-seat draft user-id)
        updated-seat (-> seat
                         (update :packs rest)
                         (update :packs vec)
                         (update-in [:player :picks] conj pick))
        updated-next-seat (update next-seat :packs
                                  #(cond-> % (not-empty picked-pack) (conj picked-pack)))
        updated-draft (-> draft
                          (update :seats swap-seat updated-seat)
                          (update :seats swap-seat updated-next-seat)
                          (#(if (packs-empty? %) (next-pack %) %)))
        resulting-messages (pick-results updated-draft user-id)
        ]

    ))
