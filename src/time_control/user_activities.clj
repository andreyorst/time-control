(ns time-control.user-activities
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [me.raynes.fs :as fs]
            [time-control.log :refer [*log-dir*]]))

(def ^:dynamic *activities-file* (fs/file *log-dir* ".commands.edn"))


;; Cached read of activity config file.  If file modification time is
;; newer than modification time stored in the cache, re reads
;; information from the disk.  Otherwise uses data from the cache, if
;; available. Note, this may look like a variable, but this is a
;; function, that is created with the #() notation.
(def user-activities
  (let [cache (atom {:mod-time 0
                     :config {}})]
    #(let [mod-time (fs/mod-time *activities-file*)]
       (when (> mod-time (:mod-time @cache))
         (swap! cache assoc
                :config (edn/read-string (slurp *activities-file*))
                :mod-time mod-time))
       (:config @cache))))


(defn add-activity
  "Adds activity `name` and `descriotions` to the activity config."
  [[name & descriptions]]
  (let [acts (assoc (user-activities) name (str/join " " descriptions))]
    (spit *activities-file* (with-out-str (pp/pprint acts)))))


(defn remove-activity
  "Removes activity `name` from the activity config."
  [[name]]
  (let [acts (dissoc (user-activities) name)]
    (spit *activities-file* (with-out-str (pp/pprint acts)))))


(defn list-activities
  "Prints all available activities."
  [& _]
  (doseq [[activity descr] (sort-by first compare (user-activities))]
    (println activity "-" descr)))
