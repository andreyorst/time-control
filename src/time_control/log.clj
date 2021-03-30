(ns time-control.log
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [java-time :as t]
            [me.raynes.fs :as fs]
            [time-control.user-activities :refer [user-activities]]
            [flatland.ordered.map :refer [ordered-map]])
  (:import [java.time ZonedDateTime]))

(def ^:dynamic *log-dir* (str (fs/file (fs/home) "time-log")))

(defonce log-file (atom nil))
(defonce log (atom []))

(def ^:const date-re
  (re-pattern (str #"(?:(?:(?:0[1-9]|1[0-9]|2[0-8])-(?:0[1-9]|1[012]))|(?:29|30|31)-(?:0[13578]|1[02])|(?:29|30)-(?:0[4,6,9]|11))-(?:19|[2-9][0-9])\d\d"
                   #"|(?:29-02-(?:19|[2-9][0-9])(?:00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96))")))

(defn- extract-log-date-id
  "Extracts date string and optional index from `file`."
  [file]
  (->> file
       str
       (re-find (re-pattern (str "(" date-re ")(?:-(\\d+))?")))
       (drop 1)))


(defn- compare-log-names
  "Comparison of two log names based on dates and optional indexes."
  [[date1 num1] [date2 num2]]
  (or (t/before? (t/local-date "dd-MM-yyyy" date1)
                 (t/local-date "dd-MM-yyyy" date2))
      (and (= date1 date2)
           (< (if num1 (Integer/parseInt num1) 0)
              (if num2 (Integer/parseInt num2) 0)))))


(defn- get-sorted-logs
  "Finds all logs, or logs that start with `name` in the `*log-dir` and
  sorts those by date."
  ([] (get-sorted-logs nil))
  ([date]
   (let [glob (if (some? date)
                (str date "-*.time-log")
                "*.time-log")]
     (some->> glob
              (fs/glob (fs/file *log-dir*))
              (sort-by extract-log-date-id compare-log-names)))))


(defn find-previous-log
  "Get last log by date."
  ([] (last (get-sorted-logs)))
  ([date] (last (get-sorted-logs date))))


(defn- get-log-name-index
  "For logs that contain index in the name returns that index."
  [file]
  (or (some->> file
               str
               (re-find (re-pattern (str "(?:" date-re ")-(\\d+)\\.time-log$")))
               second
               Integer/parseInt)
      0))


(defn create-new-log
  "Creates new log file."
  ([] (create-new-log (t/format "dd-MM-yyyy" (t/local-date))))
  ([date]
   (let [filename (str date ".time-log")
         unique-filename (if (fs/exists? (fs/file *log-dir* filename))
                           (let [prev (find-previous-log date)]
                             (str date "-" (inc (get-log-name-index prev)) ".time-log"))
                           filename)
         log-file (fs/file *log-dir* unique-filename)]
     (doto log-file
       (spit "[]")))))


(defn- close-last-item
  "If the last item in the log doesn't have `:end` date, adds current
  date to it."
  [log]
  (let [last (dec (count log))]
    (if (and (>= last 0)
             (nth log last)
             (nil? (get-in log [last 1 :end])))
      (assoc-in log [last 1 :end] (t/java-date))
      log)))


(defn- millis-to-dhms
  "Convert `millis` to days, hours, minutes, and seconds."
  [millis]
  {:days (int (/ millis (* 1000 60 60 24)))
   :hours (int (mod (/ millis (* 1000 60 60)) 60))
   :minutes (int (mod (/ millis (* 1000 60)) 60))
   :seconds (int (mod (/ millis 1000) 60))})


(defn- millis-between
  "Gets amount of milliseconds between two dates."
  [date-a date-b]
  (Math/abs (- (t/to-millis-from-epoch date-a) (t/to-millis-from-epoch date-b))))


(defn- reduce-log-times
  "Reducing function for calculating total time for given `category`."
  [total-stats [category {:keys [start end]}]]
  (if (get total-stats category)
    (update total-stats category (fn [now start end]
                                   (+ now (millis-between start (or end (t/java-date)))))
            start end)
    (assoc total-stats category (millis-between start (or end (t/java-date))))))


(defn- log-stats
  "Full log stats."
  [log]
  (some->> log
           (reduce reduce-log-times (ordered-map))
           (map (fn [[k v]] [k (millis-to-dhms v)]))
           (into (ordered-map))))

(defn- sub-category-time [])

(defn- category-stats [log category]
  (keep #(when (= category (first %)) %) log))

(defn- beg-end-log-dates
  "Find the earliest and latest dates in the log."
  [log]
  (loop [log-start nil
         log-end nil
         lines log]
    (if lines
      (let [{:keys [start end]} (second (first lines))]
        (recur (if log-start
                 (if (.before start log-start) start log-start)
                 start)
               (let [end (or end (t/java-date))]
                 (if log-end
                   (if (.after end log-end) end log-end)
                   end))
               (next lines)))
      {:start log-start
       :end log-end})))


(defn- print-time-summary
  "Prints formatted message about time spent for given category."
  [category days hours minutes seconds]
  (println (str category ": " (cond-> ""
                                (> days 0) (str days "d ")
                                (> hours 0) (str hours "h ")
                                (> minutes 0) (str minutes "m ")
                                (> seconds 0) (str seconds "s ")))))


(defn- print-log-start-end [log]
  (let [{:keys [start end]} (beg-end-log-dates log)]
    (println (str "Activity summary from "
                  (t/format "dd-MM-yyyy HH:mm:ss" (ZonedDateTime/ofInstant (.toInstant start) (t/zone-id)))
                  " to "
                  (t/format "dd-MM-yyyy HH:mm:ss" (ZonedDateTime/ofInstant (.toInstant end) (t/zone-id)))))))


(defn- print-log-summary'
  "Prints time statistics for the given log."
  [log prefix]
  (if (seq log)
    (let [total-stats (log-stats log)]
      (doseq [[category {:keys [days hours minutes seconds]
                         :or {days 0 hours 0 minutes 0 seconds 0}}] total-stats]
        (print-time-summary (str prefix category) days hours minutes seconds)))
    (when @log-file
      (println (str prefix "No data")))))


(defn log-summary
  "Public API for printing activity stats only for the current log."
  []
  (let [log @log]
    (print-log-start-end log)
    (print-log-summary' log "")))


(defn last-log-item-summary
  "Prints activity summary for last log item."
  []
  (let [log @log]
    (if (seq log)
      (let [[category {:keys [descr]} :as last-line] (last log)
            row-stats (log-stats [last-line])
            {:keys [days hours minutes seconds]
             :or {days 0 hours 0 minutes 0 seconds 0}} (first (vals row-stats))]
        (print-time-summary
         (if-not (empty? descr)
           (str category " - " descr)
           category)
         days hours minutes seconds))
      (when @log-file
        (println "No data")))))


(defn log-activity
  "Adds new activity to the log.  Closes previous activity if not
  closed."
  [type descr]
  (println "Started new activity:"
           (if-not (empty? descr) (str type " - " descr) type))
  (swap! log close-last-item)
  (swap! log conj [type {:descr descr
                         :start (t/java-date)}]))

(defn write-current
  "Store current log on the file system."
  []
  (when-some [log-file @log-file]
    (some->> @log
             close-last-item
             (#(with-out-str (pp/pprint %)))
             (spit log-file))))


(defn update-last-log-entry
  "Change the category and description of the last log item."
  [[category & descriptions]]
  (let [l @log
        last (dec (count l))]
    (when-let [line (and (>= last 0) (nth l last))]
      (println "Updated last activity to"
               (if-not (empty? category)
                 (str category " - " (str/join " " descriptions))
                 category))
      (swap! log assoc last (-> line
                                (assoc-in [0] category)
                                (assoc-in [1 :descr] (str/join " " descriptions)))))))


(defn- gather-logs-in-period
  "Get all log files for the given `period`.  If the `period` is `all`
  gathers all logs in the `*log-dir*`.  If the `period` is date in the
  format of `dd-MM-yyyy` gathers the logs for this day only.  If the
  `period` is two dates separated with colon, gathers logs for dates
  in this period.  Otherwise gets all logs for current day."
  [period]
  (try
    (cond (= period "all")
          (into []
                (comp (map #(edn/read-string (slurp %)))
                      cat)
                (get-sorted-logs))

          (and period
               (or (re-find (re-pattern (str date-re ":" date-re)) period)
                   (re-find date-re period)))
          (let  [[start end] (str/split period #":")
                 end (t/local-date "dd-MM-yyyy" (or end start))
                 start (t/local-date "dd-MM-yyyy" start)
                 logs (get-sorted-logs)]
            (into []
                  (comp (filter #(let [[date] (extract-log-date-id %)]
                                   (not (t/before? (t/local-date "dd-MM-yyyy" date) start))))
                        (filter #(let [[date] (extract-log-date-id %)]
                                   (not (t/after? (t/local-date "dd-MM-yyyy" date) end))))
                        (map #(edn/read-string (slurp %)))
                        cat)
                  logs))

          (nil? period)
          (gather-logs-in-period (t/format "dd-MM-yyyy" (t/local-date)))

          :else
          (println "Unsupported period. Use empty period, date, beg-date:end-date, or all"))
    (catch Exception e
      (println "Error during reading logs" (.getMessage e)))))

(defn log-summary-for-period
  "Prints total log summary for given `period`."
  [[period]]
  (write-current)
  (print-log-summary' (gather-logs-in-period period) ""))

(defn- filter-by-category [log category]
  (sequence (comp (keep #(when (= category (first %)) %))
                  (map #(let [m (second %)
                              category (:descr m)
                              m (dissoc m :descr)]
                          [category m])))
            log))


(defn- detailed-log-summary' [log category]
  (if-let [category-log (seq (filter-by-category log category))]
    (let [{:keys [days hours minutes seconds]
           :or {days 0 hours 0 minutes 0 seconds 0}}
          (get (log-stats log) category)]
      (println (str "Stats for category " category ":"))
      (print-log-summary' category-log "  ")
      (when (pos-int? (+ days hours minutes seconds))
        (print (str "    " category " time summary"))
        (print-time-summary "" days hours minutes seconds)))
    (println "No stats for category" category)))


(defn detailed-log-summary
  "Prints detailed log summary for given `category` and `period`."
  [[category period]]
  (let [logs (gather-logs-in-period period)]
    (print-log-start-end logs)
    (case category
      "all" (doseq [category (vals (user-activities))]
              (detailed-log-summary' logs category))
      (detailed-log-summary' logs category))))
