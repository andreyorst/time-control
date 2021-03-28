(ns time-control.core
  (:require [clojure.edn :as edn]
            [clojure.pprint]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [me.raynes.fs :as fs]
            [time-control.log :refer [*log-dir*
                                      log
                                      log-file
                                      write-current
                                      log-summary-for-period
                                      last-log-item-summary
                                      update-last-log-entry
                                      log-activity
                                      create-new-log
                                      find-previous-log]]
            [time-control.user-activities :refer [add-activity remove-activity user-activities list-activities]])
  (:gen-class))

(def ^:dynamic *print-interval* 3000000)

(defonce printer (atom nil))
(defonce saver (atom nil))


(defmacro periodic-job
  "Start background job `body` that runs every `period` milliseconds.
  Stores the future in the given `atom`, and cancels previous future
  if the `atom` contains one."
  {:style/indent 2}
  [atom period & body]
  `(let [atom# (deref ~atom)]
     (when (future? atom#)
       (future-cancel atom#))
     (reset! ~atom (future
                     (while (not (Thread/interrupted))
                       (Thread/sleep ~period)
                       ~@body)))))


(defmacro with-term-prompt
  "Wraps the `body` in a way that helps maintaining visual look of the
  log in the terminal."
  [& body]
  `(do (println)
       ~@body
       (print "> ")
       (flush)))


(defn read-input []
  (print "> ")
  (flush)
  (read-line))


(defn quit
  "Cancel any running background jobs, write log file and exit the
  program."
  []
  (future-cancel @printer)
  (future-cancel @saver)
  (write-current)
  (System/exit 0))


;; Each command contains a description `:descr`, and function `:func`
;; to run.  Optional `:args`, `:req-args`, and `:opt-args` are used to
;; enforce minimal correct amount of arguments, and build help message.
(def commands
  {":h" {:descr "Help for available commands"
         :func (fn [& _]
                 (doseq [[key {:keys [descr req-args opt-args]}] commands]
                   (println (str key
                                 (if opt-args
                                   (str " [" (str/join "] [" opt-args) "]")
                                   "")
                                 (if req-args
                                   (str " <" (str/join "> <" req-args) ">")
                                   "")
                                 " - " descr))))}

   ":n" {:descr "Start new log"
         :func (fn [& _]
                 (if-let [log-file @log-file]
                   (println "Log is already selected:" (str log-file))
                   (let [file (create-new-log)]
                     (println "Created new log:" (str file))
                     (reset! log-file file)
                     (log-activity "unknown" "unknown activity")
                     (periodic-job printer *print-interval*
                       (with-term-prompt (last-log-item-summary))))))}

   ":p" {:descr "Continue previous log"
         :func (fn [& _]
                 (if-let [log-file @log-file]
                   (println "Log is already selected:" (str log-file))
                   (let [file (find-previous-log)]
                     (println "Selected log:" (str file))
                     (reset! log-file file)
                     (reset! log (edn/read-string (slurp file)))
                     (log-activity "unknown" "unknown activity")
                     (periodic-job printer *print-interval*
                       (with-term-prompt (last-log-item-summary))))))}

   ":c" {:descr "Current activity stats"
         :func (fn [& _] (last-log-item-summary))}

   ":e" {:descr "Exit"
         :func (constantly :exit)}

   ":s" {:descr "Summary for given period"
         :opt-args ["date|start-date:end-date|all"]
         :func log-summary-for-period}

   ":a" {:descr "Add new activity type"
         :req-args ["name" "description*"]
         :func add-activity}

   ":r" {:descr "Remove activity type"
         :req-args ["name"]
         :func remove-activity}

   ":u" {:descr "Update last activity description and type"
         :req-args ["activity" "description*"]
         :func update-last-log-entry}

   ":l" {:descr "List all available activities"
         :func (fn [& _] (list-activities))}})


(defn execute
  "Execute `cmd` with `args`.
  If `cmd` starts with colon, and is available in the `commands` map,
  execute respective `:func` wiht `args`.

  If `cmd` is not available in the `commands` map, looks it as an
  activity in `user-activities`, and starts new activity if found.

  Otherwise prints error."
  [[cmd & args]]
  (if-let [command (get commands cmd)]
    (let [{:keys [func req-args]} command]
      (if (or (nil? req-args) (>= (count args) (count req-args)))
        (func args)
        (println (str "Wrong argument amount: " (count args)
                      " for " cmd " (" (count req-args) ")"))))
    (if @log-file
      (if-let [activity (get (user-activities) cmd)]
        (do (log-activity activity (str/join " " args))
            (periodic-job printer *print-interval*
              (with-term-prompt (last-log-item-summary))))
        (when (not-empty cmd)
          (if (str/starts-with? cmd ":")
            (do (println (str "Unknown command '" cmd "'"))
                (println ":h - list all commands"))
            (do (println (str "unknown activity '" cmd "'"))
                (println ":l - list all activities")))))
      (do (println "No log active")
          (println ":n - create new log")
          (println ":p - continue last log")))))


(defn run
  "Starts two background jobs for logging current activity, and saving
  log file, and one foreground job for user input."
  [{:keys [save-interval]}]
  (periodic-job saver save-interval
    (write-current))
  (loop []
    (if-some [input (read-input)]
      (case (-> input
                (str/split #"\s")
                execute)
        :exit (quit)
        (recur))
      (quit))))


(def arg-opts
  [["-p"
    "--print-interval TIME"
    "Interval in minutes to report current activity status"
    :default 300000
    :default-desc "5"
    :parse-fn (fn [x]
                (-> x
                    (edn/read-string)
                    (* 60 1000)))]
   ["-s"
    "--save-interval TIME"
    "Interval in minutes to save activity log"
    :default 60000
    :default-desc "1"
    :parse-fn (fn [x]
                (-> x
                    (edn/read-string)
                    (* 60 1000)))]
   [nil
    "--log-dir PATH"
    "Path to the log directory"
    :default (str (fs/file (fs/home) "time-log"))
    :default-desc "~/time-log"
    :parse-fn #(str (fs/file (fs/expand-home %)))
    :validate [#(not (fs/file? %))
               "File with this name already exists"]]
   ["-h" "--help" "Print this message and exit"]])


(defn -main
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args arg-opts)
        {:keys [log-dir help]} options]
    (when errors
      (doseq [error errors]
        (println error))
      (System/exit 1))
    (when help
      (println summary)
      (System/exit 0))
    (binding [*log-dir* log-dir
              *print-interval* (:print-interval options)]
      (when-not (fs/exists? *log-dir*)
        (fs/mkdir *log-dir*))
      (run options))))
