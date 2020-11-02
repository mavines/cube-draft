(ns cube-bot.db
  (:require ["mongodb" :refer (MongoClient)]
            [cljs.nodejs :as node]))

(def url "mongodb://localhost:27017")
(def dbName "cube-bot")
(defonce drafts "drafts")
(defonce *client (atom nil))
(defonce *db (atom nil))

(defonce *connected (atom false))

(defn connect [url]
  (reset! *client (MongoClient. url))
  (.connect @*client #(do (reset! *connected true)
                          (reset! *db (.db @*client dbName)))))

(defn disconnect []
  (when @*client (.close @*client))
  (reset! *client nil))

(defn save-draft [draft]
  (when-let [^js db @*db]
    (let [^js drafts-collection (.collection db drafts)]
      (.insert drafts-collection draft #(println %1 " " %2)))))

(defn get-draft [id]
  (when-let [^js db @*db]
    (let [^js drafts-collection (.collection db drafts)]
      (.toArray (.find drafts-collection (clj->js {:draft-id "1234"})) #(println %1 " " %2)))))

(defn update-draft [id]
  (when-let [^js db @*db]
    (let [^js drafts-collection (.collection db drafts)]
      (.updateOne drafts-collection
                  (clj->js {:draft-id id})
                  (clj->js {:$set {:name "My Draft Triple Updated"}})
                  #(println %1 " " %2)))))
