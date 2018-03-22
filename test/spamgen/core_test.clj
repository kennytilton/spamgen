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

#_
(deftest devtest
  (profile
    {:dynamic? true}

    (p ::send-50k
      (email-batch-to-sendfiles
        (email-records-test-gen 50000)))))

(deftest infile-edn
  (pln :startedn)
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader "bulkinput/emf-10.edn"))]
    (pln :ednin in)
    (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
      (pln :eseq (take-while (partial not= :fini) edn-seq))
      (email-batch-to-sendfiles (take-while (partial not= :fini) edn-seq)))))

#_(deftest devtest-duped
    (email-batch-to-sendfiles
      (email-records-duped-test-gen (:total-email-ct env))))