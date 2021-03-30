(ns time-control.user-activities-test
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [me.raynes.fs :as fs]
            [time-control.user-activities :as acts]
            [time-control.log :refer [*log-dir*]]))

(def test-activities {"a" "anime"
                      "h" "housework"
                      "e" "exercise"
                      "t" "training"})


(defn- wrap-activity-file [test]
  (binding [*log-dir* (fs/file "test" "data" "time-log")
            acts/*activities-file* (fs/file "test" "data" "time-log" ".commands.edn")]
    (fs/mkdirs *log-dir*)
    (spit acts/*activities-file* (with-out-str (pp/pprint test-activities)))
    (test)
    (fs/delete acts/*activities-file*)
    (fs/delete-dir *log-dir*)))


(defn- with-logging-slurp [test]
  (with-redefs [slurp (fn [f]
                        (print "reading file")
                        (let [sw (java.io.StringWriter.)]
                          (with-open [^java.io.Reader r (io/reader f)]
                            (io/copy r sw)
                            (.toString sw))))]
    (test)))


(use-fixtures :each
  wrap-activity-file
  with-logging-slurp)


(deftest caching
  (testing "file is not read from disk twice"
    (is (= "reading file"
           (with-out-str (acts/user-activities))))
    (is (= "" (with-out-str (acts/user-activities)))))

  (testing "file is read upon modification"
    (is (= "" (with-out-str (acts/user-activities))))
    (spit acts/*activities-file* "{}")
    (is (= "reading file"
           (with-out-str (acts/user-activities))))
    (is (= {} (acts/user-activities)))))


(deftest adding-removing-activities
  (testing "adding activities"
    (is (= (assoc test-activities "1" "2")
           (do (acts/add-activity ["1" "2"])
               (acts/user-activities)))))

  (testing "removing activities"
    (is (= (dissoc test-activities "1")
           (do (acts/remove-activity ["1"])
               (acts/user-activities))))))


(deftest activity-printing
  (testing "listing activities"
    (is (= "reading filea - anime\ne - exercise\nh - housework\nt - training\n"
           (with-out-str (acts/list-activities))))))
