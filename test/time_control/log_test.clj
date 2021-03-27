(ns time-control.log-test
  (:require [time-control.log :as log-file :refer [*log-dir*]]
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
      (spit (fs/file *log-dir* file) "vaiv"))
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
           (str (log-file/find-previous-log)))))

  (testing "create new log file"
    (is (= (str (fs/file *log-dir* "01-01-2021-11.time-log"))
           (str (log-file/create-new-log "01-01-2021"))))
    (is (= (str (fs/file *log-dir* "01-02-2021.time-log"))
           (str (log-file/create-new-log "01-02-2021"))))
    (is (= (str (fs/file *log-dir* "01-02-2021-1.time-log"))
           (str (log-file/create-new-log "01-02-2021")))))

  (testing "get last log for specific date"
    (is (= (str (fs/file *log-dir* "01-01-2021-11.time-log"))
           (str (log-file/find-previous-log "01-01-2021"))))
    (is (= (str (fs/file *log-dir* "01-02-2021-1.time-log"))
           (str (log-file/find-previous-log "01-02-2021"))))))
