(defproject time-control "0.1.0-SNAPSHOT"
  :description "Simple time logging utility"
  :url "https://gitlab.com/andreyorst/time-control"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[io.taylorwood/lein-native-image "0.3.1"]]
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [clj-commons/fs "1.6.307"]
                 [org.clojure/tools.cli "1.0.206"]
                 [org.flatland/ordered "1.5.9"]]
  :main ^:skip-aot time-control.core
  :target-path "target/%s"
  :native-image {:name      "time-control"
                 :graal-bin "/opt/graalvm-ce-java11-21.0.0.2/bin"
                 :opts      ["--verbose"
                             "--no-fallback"
                             "--initialize-at-build-time"
                             "-H:+ReportExceptionStackTraces"
                             "--report-unsupported-elements-at-runtime"
                             ]}
  :profiles {:uberjar {:aot :all
                       :native-image {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}})
