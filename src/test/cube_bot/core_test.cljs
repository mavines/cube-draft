(ns cube-bot.core-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures async]]
            [clojure.pprint :refer [pprint]]
            [cube-bot.core :as core]
            [cube-bot.cube :as cube]))

(defonce small-cube (take 33 cube/combo))
(defonce small-draft (core/build-draft small-cube [123 456]))

(defonce medium-cube (take 86 cube/combo))
(defonce medium-draft (core/build-draft medium-cube [123 456 789]))

(deftest build-seat-test
  (let [pack (take 15 small-cube)
        seat (core/build-seat 123 pack)]
    (is (= {:id 123 :picks []}
           (:player seat)))
    (is (= pack
           (first (:packs seat))))
    (is (= 1 (count (:packs seat))))))

(deftest build-draft-test
  (let [draft (core/build-draft small-cube [123 456])
        first-seat (first (:seats draft))]
    (is (= 2 (count (:seats draft))))
    (is (= {:id 123 :picks []}
           (:player first-seat)))
    (is (= 15 (count (first (:packs first-seat)))))
    (is (not= (first (:packs first-seat))
              (first (:packs (second (:seats draft))))))))

(deftest pick-card-test
  ;; User 123 picks card 0
  (let [after-pick (core/perform-pick small-draft 123 0)
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
                        (core/perform-pick 123 0)
                        (core/perform-pick 456 3))
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
  (let [picked-draft (core/perform-pick small-draft 123 0)]
    (is (not (core/send-next-pack? picked-draft 123)))))

(deftest send-neighbor-pack?-test
  (let [picked-draft (core/perform-pick small-draft 123 0)
        picked-back (core/perform-pick picked-draft 456 0)]
    (is (not (core/send-neighbor-pack? picked-draft 123)))
    (is (core/send-next-pack? picked-back 456))
    (is (core/send-neighbor-pack? picked-back 456))))


(deftest pick-results-test
  (let [picked-draft (core/perform-pick small-draft 123 0)
        double-picked-draft (core/perform-pick picked-draft 456 0)
        expected-result [{:command :dm
                          :user-id 456
                          :message (core/pack->text (rest (first (partition 15 small-cube))))}
                         {:command :dm
                          :user-id 123
                          :message (core/pack->text (rest (second (partition 15 small-cube))))}]
        result (core/pick-results double-picked-draft 456)]
    (is (= expected-result result))))

;; 3 players
;; Player 1 picks - no messages
;; Player 2 picks - message to 2
;; Player 3 picks - message to 3 and 1
(deftest pick-results-test-3player
  (let [one-pick-draft (core/perform-pick medium-draft 123 0)
        two-pick-draft (core/perform-pick one-pick-draft 456 0)
        three-pick-draft (core/perform-pick two-pick-draft 789 0)
        expected-result [{:command :dm
                          :user-id 789
                          :message (core/pack->text (rest (second (partition 15 medium-cube))))}
                         {:command :dm
                          :user-id 123
                          :message (core/pack->text (rest (nth (partition 15 medium-cube) 2)))}]
        result (core/pick-results three-pick-draft 789)]
    (is (= expected-result result))))
