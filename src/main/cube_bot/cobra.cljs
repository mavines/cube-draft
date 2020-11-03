(ns cube-bot.cobra
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [clojure.walk :refer [stringify-keys]]
            [xmlhttprequest :refer [XMLHttpRequest]])
  (:require-macros [cljs.core.async.macros :refer [go]]))
(set! js/XMLHttpRequest XMLHttpRequest)

(defonce endpoint "https://cubecobra.com/cube/api/cubecardnames/")
(defonce small-id "5fa0185133dbe856b9789d33")


(defn build-word [entry]
  (let [char (key entry)
        subwords (flatten (map build-word (val entry)))]
    (if (= char "$")
      ""
      (map #(str char %) subwords))))

(defn tree->words [tree]
  (flatten (map build-word tree)))


(defn get-cube [id handler]
  (go (let [response (<! (http/get (str endpoint id) {:timeout 5000}))
            success? (:success response)
            cardnames (-> response :body :cardnames stringify-keys tree->words)]
        (if success?
          (handler nil cardnames)
          (handler (str "Error fetching cube: " id) nil)))))
