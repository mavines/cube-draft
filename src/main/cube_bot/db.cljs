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
      (.insertOne drafts-collection (clj->js draft)))))

(defn fix-draft [db-draft]
  (-> db-draft
      (update :num-packs js/parseInt)
      (update :pack-number js/parseInt)))

(defn get-draft [id callback]
  (when-let [^js db @*db]
    (let [^js drafts-collection (.collection db drafts)]
      (.findOne drafts-collection (clj->js {:draft-id id})
                #(if (nil? %2)
                   (callback (str "Error getting draft: " id) nil)
                   (callback %1 (fix-draft (js->clj %2 :keywordize-keys true))))))))

(defn update-draft [draft]
  (when-let [^js db @*db]
    (let [^js drafts-collection (.collection db drafts)]
      (.replaceOne drafts-collection
                  (clj->js {:draft-id (:draft-id draft)})
                  (clj->js draft)))))
