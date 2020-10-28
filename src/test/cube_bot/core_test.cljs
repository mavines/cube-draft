(ns cube-bot.core-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures async]]
            [cube-bot.core :as core]
            [cube-bot.cube :as cube]))

(defonce small-cube (take 33 cube/combo))
(defonce small-draft (core/build-draft small-cube [123 456]))

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
