(ns time-control.log-test
  (:require [time-control.log :as log :refer [*log-dir*]]
            [me.raynes.fs :as fs]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(defn- wrap-log-dir [test]
  (binding [*log-dir* "test/data/time-log/"]
    (test)))

(defn- prepare-logs [test]
  (let [files (conj (map str
                         (repeat "01-01-2021-")
                         (range 10 0 -1)
                         (repeat ".time-log"))
                    "01-01-2021.time-log")]
    (fs/mkdirs *log-dir*)
    (doseq [file files]
      (spit (fs/file *log-dir* file) "[]"))
    (test)
    (doseq [file files]
      (fs/delete (fs/file *log-dir* file)))
    (fs/delete-dir *log-dir*)))

(use-fixtures :each
  wrap-log-dir
  prepare-logs)

(deftest log-files
  (testing "get last log file"
    (is (= (str (fs/file *log-dir* "01-01-2021-10.time-log"))
           (str (log/find-previous-log)))))

  (testing "create new log file"
    (is (= (str (fs/file *log-dir* "01-01-2021-11.time-log"))
           (str (log/create-new-log "01-01-2021"))))
    (is (= (str (fs/file *log-dir* "01-02-2021.time-log"))
           (str (log/create-new-log "01-02-2021"))))
    (is (= (str (fs/file *log-dir* "01-02-2021-1.time-log"))
           (str (log/create-new-log "01-02-2021")))))

  (testing "get last log for specific date"
    (is (= (str (fs/file *log-dir* "01-01-2021-11.time-log"))
           (str (log/find-previous-log "01-01-2021"))))
    (is (= (str (fs/file *log-dir* "01-02-2021-1.time-log"))
           (str (log/find-previous-log "01-02-2021"))))))

(deftest date-test
  (testing "invalid dates"
    (is (nil? (re-find log/date-re "1-1-2000")))
    (is (nil? (re-find log/date-re "32-01-2000")))
    (is (nil? (re-find log/date-re "12-13-2000")))
    (is (nil? (re-find log/date-re "00-00-0000"))))

  (testing "leap years"
    (doseq [year (range 1900 2400 4)]
      (is (re-find log/date-re (str "29-02-" year))))))

(def log
  [["programming"
    {:descr "time-control",
     :start #inst "2021-03-28T07:05:14.488-00:00",
     :end #inst "2021-03-28T07:15:16.451-00:00"}]
   ["programming"
    {:descr "time-control",
     :start #inst "2021-03-28T07:17:58.789-00:00",
     :end #inst "2021-03-28T07:19:43.537-00:00"}]
   ["home"
    {:descr "housekeeping",
     :start #inst "2021-03-28T08:20:42.926-00:00",
     :end #inst "2021-03-28T08:21:52.395-00:00"}]
   ["home"
    {:descr "cleaning",
     :start #inst "2021-03-28T08:22:01.431-00:00",
     :end #inst "2021-03-28T08:41:39.222-00:00"}]
   ["programming"
    {:descr "time-control",
     :start #inst "2021-03-28T08:41:39.222-00:00",
     :end #inst "2021-03-28T08:41:52.204-00:00"}]
   ["programming"
    {:descr "time-control tests",
     :start #inst "2021-03-28T08:42:59.636-00:00",
     :end #inst "2021-03-28T08:43:48.782-00:00"}]
   ["programming"
    {:descr "learning clojure",
     :start #inst "2021-03-28T08:43:57.638-00:00",
     :end #inst "2021-03-28T09:01:37.087-00:00"}]])

(deftest log-stats
  (testing "category filtering"
    (is (= [["time-control"
             {:start #inst "2021-03-28T07:05:14.488-00:00",
              :end #inst "2021-03-28T07:15:16.451-00:00"}]
            ["time-control"
             {:start #inst "2021-03-28T07:17:58.789-00:00",
              :end #inst "2021-03-28T07:19:43.537-00:00"}]
            ["time-control"
             {:start #inst "2021-03-28T08:41:39.222-00:00",
              :end #inst "2021-03-28T08:41:52.204-00:00"}]
            ["time-control tests"
             {:start #inst "2021-03-28T08:42:59.636-00:00",
              :end #inst "2021-03-28T08:43:48.782-00:00"}]
            ["learning clojure"
             {:start #inst "2021-03-28T08:43:57.638-00:00",
              :end #inst "2021-03-28T09:01:37.087-00:00"}]]
           (#'log/filter-by-category log "programming")))
    (is (= [["housekeeping"
             {:start #inst "2021-03-28T08:20:42.926-00:00",
              :end #inst "2021-03-28T08:21:52.395-00:00"}]
            ["cleaning"
             {:start #inst "2021-03-28T08:22:01.431-00:00",
              :end #inst "2021-03-28T08:41:39.222-00:00"}]]
           (#'log/filter-by-category log "home")))
    (is (= []
           (#'log/filter-by-category log "non existing"))))

  (testing "log stats by category"
    (is (= {"time-control" {:days 0, :hours 0, :minutes 11, :seconds 59},
            "time-control tests" {:days 0, :hours 0, :minutes 0, :seconds 49},
            "learning clojure" {:days 0, :hours 0, :minutes 17, :seconds 39}}
           (#'log/log-stats (#'log/filter-by-category log "programming"))))
    (is (= {"housekeeping" {:days 0, :hours 0, :minutes 1, :seconds 9},
            "cleaning" {:days 0, :hours 0, :minutes 19, :seconds 37}}
           (#'log/log-stats (#'log/filter-by-category log "home")))))

  (testing "total log stats"
    (is (= {"programming" {:days 0, :hours 0, :minutes 30, :seconds 28},
            "home" {:days 0, :hours 0, :minutes 20, :seconds 47}}
           (#'log/log-stats log)))))
