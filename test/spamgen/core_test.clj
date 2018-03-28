(ns spamgen.core-test
  (:require
    [clojure.test :refer :all]
    [config.core :refer [env]]
    [clojure.string :as str]
    [clojure.edn :as edn]
    [clojure.core.async :refer [chan go-loop <! <!! >!!
                                timeout alt!!]]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
    [spamgen.core :refer :all]
    [spamgen.genlist :refer :all]))

(tufte/add-basic-println-handler! {})


#_(with-open [in (java.io.PushbackReader. (clojure.java.io/reader "bulkinput/em-12.edn"))]
    (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
      (doseq [chunk (take-while (partial not= :fini) edn-seq)]
        (pln :chk chunk))))

#_(edn/read-string
    (slurp "bulkinput/em-1000.edn"))

(deftest slurptest
  (profile
    {:dynamic? true}
    (p :devtest-mp
      (email-file-to-sendfiles-mp
        "bulkinput/em-100000-100.edn"
        false)))
  (pln :devtest-fini))

(deftest slurptest-unprof
  (let [wait (atom (System/currentTimeMillis))]
    (email-file-to-sendfiles-mp
      "bulkinput/em-200000-100.edn"
      false)
    (pln :slurptest-took (- (System/currentTimeMillis) @wait))))

;; todo add tests to read back in output and confirm constraints met

