(ns cube-bot.util)

(defn jsprint [js]
  (.log js/console js))
(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))
