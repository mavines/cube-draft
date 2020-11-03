(ns cube-bot.polyfills
  (:require ["@peculiar/webcrypto" :refer [Crypto]]))

(set! js/crypto (Crypto.))
