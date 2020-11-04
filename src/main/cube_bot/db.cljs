(ns cube-bot.db
  (:require ["mongodb" :refer (MongoClient)]
            [cljs.core.async :as a :refer [<! >! go put! chan]]
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
    (let [out (chan)
          ^js drafts-collection (.collection db drafts)]
      (.insertOne drafts-collection (clj->js draft)
                  (fn [err res]
                    (cond
                      (nil? res) (put! out {:error (str "Error saving draft: " (:draft-id draft))})
                      (some? err) (put! out {:error (str "Error saving draft: " (:draft-id draft) " " err)})
                      :else (put! out {:success "Draft saved"})))))))

(defn fix-draft [db-draft]
  (-> db-draft
      (update :num-packs js/parseInt)
      (update :pack-number js/parseInt)))

(defn get-draft [id]
  (when-let [^js db @*db]
    (let [out (chan)
          ^js drafts-collection (.collection db drafts)]
      (.findOne drafts-collection (clj->js {:draft-id id})
                (fn [err res]
                  (cond
                    (nil? res) (put! out {:error (str "Error getting draft: " id)} )
                    (some? err) (put! out {:error (str "Error getting draft: " id " " err)})
                    :else (put! out (fix-draft (js->clj res :keywordize-keys true))))))
      out)))

(defn update-draft [draft]
  (when-let [^js db @*db]
    (let [out (chan)
          ^js drafts-collection (.collection db drafts)
          id (:draft-id draft)]
      (.replaceOne drafts-collection
                   (clj->js {:draft-id id})
                   (clj->js draft)
                   (fn [err res]
                     (cond
                       (nil? res) (put! out {:error ("Error updating draft: " id)})
                       (some? err) (put! out {:error ("Error updating draft: " id " " err)})
                       :else (put! out {:success "Draft updated"}))))
      out)))
