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

(deftest devtest
  (profile
    {:dynamic? true}

    (p :devtest-mp
      (email-stream-to-sendfiles-mp
        (email-records-test-gen 4000))))
  (pln :devtest-fini))

;; todo add tests to read back in output and confirm constraints met

#_(deftest infile-batch-edn
    (email-batch-to-sendfiles "bulkinput/emf-1000.edn")
    #_(with-open [in (java.io.PushbackReader. (clojure.java.io/reader "bulkinput/emf-1000.edn"))]
        (pln :ednin in)
        (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
          (pln :eseq (take-while (partial not= :fini) edn-seq))
          (email-batch-to-sendfiles (take-while (partial not= :fini) edn-seq)))))

#_(deftest infile-stream-edn
    (with-open [in (java.io.PushbackReader. (clojure.java.io/reader "bulkinput/emf-100.edn"))]
      (pln :ednin in)
      (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
        ;; (pln :eseq (take-while (partial not= :fini) edn-seq))
        (email-stream-to-sendfiles (take-while (partial not= :fini) edn-seq))
        #_(doseq [erec (take-while (partial not= :fini) edn-seq)]
            (pln :erec erec))
        (pln :infile-stream-edn-fini))))


#_(deftest devtest-duped
    (email-batch-to-sendfiles
      (email-records-duped-test-gen (:total-email-ct env))))