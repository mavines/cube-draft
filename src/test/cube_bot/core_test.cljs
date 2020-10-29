(ns cube-bot.core-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures async]]
            [clojure.pprint :refer [pprint]]
            [cube-bot.core :as core]
            [cube-bot.cube :as cube]
            [cube-bot.draft :as draft]))

(defonce small-cube (take 33 cube/combo))
(defonce small-draft (draft/build-draft small-cube [123 456]))

(defonce medium-cube (take 86 cube/combo))
(defonce medium-draft (draft/build-draft medium-cube [123 456 789]))
