(defproject time-control "0.1.0-SNAPSHOT"
  :description "Simple time logging utility"
  :url "https://gitlab.com/andreyorst/time-control"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [clj-commons/fs "1.6.307"]
                 [clojure.java-time/clojure.java-time "0.3.2"]
                 [org.threeten/threeten-extra "1.6.0"]
                 [org.clojure/tools.cli "1.0.206"]]
  :main ^:skip-aot time-control.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
