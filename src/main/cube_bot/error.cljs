(ns cube-bot.error
  (:require [cljs.core.async :as a :refer [<! >! go put! take! chan]]
            [cube-bot.draft :as draft :refer [players-seat next-seat pick-results swap-seat next-pack packs-empty?]]))

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

(defn check [[k v] err]
  (println "check" v err)
  (if v
    [k v]
    [k {:error err}]))

(defn map-vals [m f]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn pick-from-pack [draft user-id pick-number]
  (into {} )(map check
   {:seat (:s draft)
    :pack (:pk draft)
    :player (:pl draft)
    :pick (:pi draft)}
   ["Error picking seat"
    "Pack error"
    "player error"
    "pick error"]))

(pick-from-pack {:s nil :pk nil :pl "pl" :pi "pi"} nil nil)

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
