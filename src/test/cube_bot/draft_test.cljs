(ns cube-bot.draft-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures async]]
            [clojure.pprint :refer [pprint]]
            [cube-bot.cube :as cube]
            [cube-bot.draft :as draft]))

(defonce test-cube (map str (range 200)))
(defonce number-draft (draft/build-draft test-cube [123 456 789] 2 2))
(defonce three-pack-draft (draft/build-draft test-cube [123 456 789] 3 2))

(defonce tiny-draft (draft/build-draft cube/combo [123 456 789] 2 1))

(defonce small-cube (take 33 cube/combo))
(defonce small-draft (draft/build-draft small-cube [123 456] 1 15))

(defonce medium-cube (take 154 cube/combo))
(defonce medium-draft (draft/build-draft medium-cube [123 456 789]))

(deftest build-seat-test
  (let [pack (take 15 small-cube)
        seat (draft/build-seat 123 pack)]
    (is (= {:id 123 :picks []}
           (:player seat)))
    (is (= pack
           (first (:packs seat))))
    (is (= 1 (count (:packs seat))))))

(deftest build-draft-test
  (let [draft (draft/build-draft small-cube [123 456])
        first-seat (first (:seats draft))]
    (is (= 2 (count (:seats draft))))
    (is (= {:id 123 :picks []}
           (:player first-seat)))
    (is (= 15 (count (first (:packs first-seat)))))
    (is (not= (first (:packs first-seat))
              (first (:packs (second (:seats draft))))))))

(deftest pick-card-test
  ;; User 123 picks card 0
  (let [after-pick (:draft (draft/perform-pick small-draft 123 0))
        seats (:seats after-pick)
        first-seat (first seats)
        second-seat (second seats)]
    (is (= 0 (count (:packs first-seat))))
    (is (= 2 (count (:packs second-seat))))
    (is (= 14 (-> second-seat :packs second count)))
    (is (= 1 (-> first-seat :player :picks count)))))

(deftest two-pick-card-test
  ;; User 123 picks card 0
  (let [after-picks (-> small-draft
                        (draft/perform-pick 123 0)
                        :draft
                        (draft/perform-pick 456 3)
                        :draft)
        seats (:seats after-picks)
        first-seat (first seats)
        second-seat (second seats)]
    (is (= 1 (count (:packs first-seat))))
    (is (= 1 (count (:packs second-seat))))
    (is (= 14 (-> first-seat :packs first count)))
    (is (= 14 (-> second-seat :packs first count)))
    (is (= 1 (-> first-seat :player :picks count)))
    (is (= 1 (-> second-seat :player :picks count)))))

(deftest send-next-pack?-test
  (let [picked-draft (:draft (draft/perform-pick small-draft 123 0))]
    (is (not (draft/send-next-pack? picked-draft 123)))))

(deftest send-neighbor-pack?-test
  (let [picked-draft (:draft (draft/perform-pick small-draft 123 0))
        picked-back (:draft (draft/perform-pick picked-draft 456 0))]
    (is (not (draft/send-neighbor-pack? picked-draft 123)))
    (is (draft/send-next-pack? picked-back 456))
    (is (draft/send-neighbor-pack? picked-back 456))))


(deftest pick-results-test
  (let [picked-draft (:draft (draft/perform-pick small-draft 123 0))
        {:keys [draft messages]} (draft/perform-pick picked-draft 456 0)
        expected-result [(draft/picked-message 456)
                         (draft/build-pack-message
                          (:draft-id draft)
                          (draft/players-seat draft 456))
                         (draft/build-pack-message
                          (:draft-id draft)
                          (draft/players-seat draft 123))]]
    (is (= expected-result messages))))

;; 3 players
;; Player 1 picks - message to 1
;; Player 2 picks - 2 messages to 2
;; Player 3 picks - 2 to player 3 and 1 to player 2
(deftest pick-results-test-3player
  (let [one-pick-draft (:draft (draft/perform-pick medium-draft 123 0))
        two-pick-draft (:draft (draft/perform-pick one-pick-draft 456 0))
        {:keys [draft messages]} (draft/perform-pick two-pick-draft 789 0)
        expected-result [(draft/picked-message 789)
                         (draft/build-pack-message
                          (:draft-id draft)
                          (draft/players-seat draft 789))
                         (draft/build-pack-message
                          (:draft-id draft)
                          (draft/players-seat draft 123))]]
    (is (= expected-result messages))))


(deftest last-pick-test
  (let [empty-draft (-> tiny-draft
                        (draft/perform-pick 123 0)
                        :draft
                        (draft/perform-pick 456 0)
                        :draft)
        first-player-packs (-> empty-draft :seats first :packs)
        second-player-packs (-> empty-draft :seats second :packs)]
    (is (empty? first-player-packs))
    (is (empty? second-player-packs))))

(deftest progress-pack-test
  (let [pack-2-draft (-> tiny-draft
                        (draft/perform-pick 123 0)
                        :draft
                        (draft/perform-pick 456 0)
                        :draft
                        (draft/perform-pick 789 0)
                        :draft)
        first-player-packs (-> pack-2-draft :seats first :packs)
        second-player-packs (-> pack-2-draft :seats second :packs)
        third-player-packs (-> pack-2-draft :seats (nth 2) :packs)]
    (is (= 2 (:pack-number pack-2-draft)))
    (is (= 1 (count first-player-packs)))
    (is (= 1 (count (first first-player-packs))))))

(deftest end-draft-test
  (let [pack-2-draft (-> tiny-draft
                         (draft/perform-pick 123 0)
                         :draft
                         (draft/perform-pick 456 0)
                         :draft
                         (draft/perform-pick 789 0)
                         :draft)
        draft-over (-> pack-2-draft
                       (draft/perform-pick 123 0)
                       :draft
                       (draft/perform-pick 456 0)
                       :draft
                       (draft/perform-pick 789 0))
        first-player-picks (-> draft-over :draft :seats first :player :picks)
        second-player-picks (-> draft-over :draft :seats second :player :picks)
        third-player-picks (-> draft-over :draft :seats (nth 2) :player :picks)
        messages (:messages draft-over)]
    (is (= 2 (count first-player-picks)))
    (is (= 2 (count second-player-picks)))
    (is (= 2 (count third-player-picks)))
    (is (= 4 (count messages)))))


(deftest no-double-message-on-last-pick-test
  (let [{:keys [draft messages]} (-> (draft/build-draft cube/combo [123 456] 1 3)
                                     (draft/perform-pick 123 0)
                                     :draft
                                     (draft/perform-pick 456 0)
                                     :draft
                                     (draft/perform-pick 123 0)
                                     :draft
                                     (draft/perform-pick 456 0)
                                     :draft
                                     (draft/perform-pick 123 0))]
    (is (= 1 (count messages)))))

(deftest reverse-order-test
  (let [draft (loop [d number-draft]
                (if (draft/draft-done? d)
                  d
                  (recur (-> d
                             (draft/perform-pick 123 0)
                             :draft
                             (draft/perform-pick 456 0)
                             :draft
                             (draft/perform-pick 789 0)
                             :draft))))
        first-player-picks (-> draft :seats first :player :picks)
        second-player-picks (-> draft :seats second :player :picks)
        third-player-picks (-> draft :seats (nth 2) :player :picks)]
    (is (= ["0" "5" "6" "9"] first-player-picks))
    (is (= ["2" "1" "8" "11"] second-player-picks))
    (is (= ["4" "3" "10" "7"] third-player-picks))))

(deftest last-pick-message
  (let [{:keys [draft messages]} (-> (draft/build-draft cube/combo [123 456] 1 2)
                                     (draft/perform-pick 123 0)
                                     :draft
                                     (draft/perform-pick 456 0))]
    (is (= 3 (count  messages)))))

(deftest out-of-bounds-pick
  (let [result (-> (draft/build-draft cube/combo [123 456] 1 3)
                   (draft/perform-pick 123 12))]
    (is (= "Invalid card selected from pack" (:error result)))))

(deftest pack-3-test
  (let [draft (loop [d three-pack-draft]
                (if (draft/draft-done? d)
                  d
                  (recur (-> d
                             (draft/perform-pick 123 0)
                             :draft
                             (draft/perform-pick 456 0)
                             :draft
                             (draft/perform-pick 789 0)
                             :draft))))
        first-player-picks (-> draft :seats first :player :picks)
        second-player-picks (-> draft :seats second :player :picks)
        third-player-picks (-> draft :seats (nth 2) :player :picks)]
    (is (= ["0" "5" "6" "9" "12" "17"] first-player-picks))
    (is (= ["2" "1" "8" "11" "14" "13"] second-player-picks))
    (is (= ["4" "3" "10" "7" "16" "15"] third-player-picks))))


(defonce four-player-draft (draft/build-draft cube/combo [123 456 789 "abc"] 3 15))

(deftest four-player
  (let [{:keys [draft messages]} (-> four-player-draft
                                     (draft/perform-pick "abc" 0)
                                     :draft
                                     (draft/perform-pick 123 0))]
    (is (= 2 (count messages)))))
