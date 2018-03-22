(defproject spamgen "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/test.check "0.9.0"]
                 [com.taoensso/tufte "1.4.0"]
                 [com.taoensso/timbre "4.3.1"]
                 [yogthos/config "0.8"]
                 [org.clojure/tools.cli "0.3.5"]]

  :main ^:skip-aot spamgen.core
  :target-path "target/%s"
  :bin {:name "spamgen"
        :bin-path "./bin"}

  :profiles {:uberjar {:aot :all}
             :prod {:resource-paths ["config/prod"]}
             :dev  {:resource-paths ["config/dev"]}})