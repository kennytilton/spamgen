(ns spamgen.core
  (:gen-class)
  (:require
    [config.core :refer [env]]
    [clojure.core.async :refer [chan go-loop <! <!! >!!
                                timeout alt!!]]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
    [clojure.tools.cli :refer [parse-opts] :as cli]
    ;; [clojure.java.io :as io]
    [taoensso.timbre :as log]
    [clojure.string :as str]
    [clojure.edn :as edn]
    [spamgen.genlist :refer :all]))

(def spamgen-cli
  [["-t" "--test TESTCOUNT" "Number of test email records to process ignoring file arg"
    ;;:default 100
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 1 %) "Must be a number greater than one (1)"]]

   ["-d" "--dump DUMPCOUNT" "Number of output email records to dump, per target IP"
    ;;:default 10
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 1 %) "Must be a number greater than one (1) if specified"]]

   ["-h" "--help"]])

(declare pln)

#_(-main "-h")

#_(-main "bulkinput/emf-1000.edn" "-t100" "-d10")

#_(-main "bulkinput/emf-10.edn" "-h" "-t42")

(defn -main [& args]
  ;; uncomment during development so errors get through when async in play
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (log/error {:what      :uncaught-exception
                    :exception ex
                    :where     (str "Uncaught exception on" (.getName thread))}))))

  (pln :main args)

  (let [input (cli/parse-opts args spamgen-cli)
        {:keys [options arguments summary errors]} input
        {:keys [test help]} options]

    (pln :inp input)
    (pln :options options)

    (cond
      errors (doseq [e errors]
               (println e))

      help (println "\nUsage:\n\n    spamgen <input-edn> options*\n\n"
             "Options:\n" (subs summary 1))

      :default (do
                 #_(email-batch-to-sendfiles <tbd>)
                 (println :fnyi))
      ))

  ;; WARNING: comment this out for use with REPL. Necessary, to
  ;; get standalone version to exit reliably.
  ;;
  ;; (shutdown-agents)
  )

#_(-main)

(declare email-stream-to-sendfiles
  emw-email-consider
  running-mean-ok?
  edn-dump pln)

(defn email-batch-to-sendfiles [batch-input-path]
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader batch-input-path))]
    (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
      (email-stream-to-sendfiles
        (take-while (partial not= :fini) edn-seq)))))

(defn email-stream-to-sendfiles
  "[email-stream (coll)] Produce one or more output files targeted
  for different SMTP servers constraining the sequence of emails
  in each to honor spam score constraints specified in config.edn
  and never to include two emails to the same address across all
  batches."
  [email-stream]
  (pln :stream! email-stream (first email-stream))

  (let [em-addrs-hit (ref #{})
        workers (map (fn [id smtp-ip]
                       {:id        id
                        :smtp-ip   smtp-ip
                        :ch        (chan 10)
                        :addrs-hit em-addrs-hit
                        :out-file  (str
                                     (:bulkmail-out-path env) "/"
                                     smtp-ip ".txt")
                        :stats     (atom {:em-ct        0
                                          :running-mean 0
                                          :score-sum    0})})
                  (range)
                  (take (min (count (:smtp env))
                          (:worker-ct env))
                    (:smtp env)))

        work-procs (dorun
                     (map (fn [w]
                            (go-loop []
                              (if-let [task (<! (:ch w))]
                                (do
                                  (emw-email-consider w task)
                                  (recur))
                                (do
                                  (pln :worker-fini (:id w))
                                  [:fini (:id w)]))))
                       workers))]

    ;; --- initialize spit files for latter appends, including now a header

    (pln :spit-init)

    (doseq [w workers]
      (spit (:out-file w)
        {:run-date (.toString (java.util.Date.))
         :smtp-ip  (:smtp-ip w)}))

    (pln :feeding)

    (p :feed-workers
      (dorun
        (map (fn [worker em-rec]
               (>!! (:ch worker) em-rec))
          (cycle workers)
          email-stream)))

    (pln :waiting-on-workers)

    (loop [[p1 & rp :as ps] work-procs]
      (when p1
        (when-let [out (alt!!
                         (timeout 100) :timeout
                         p1
                         ([r] r))]
          (recur rp))))

    (println :fini)

    #_(doseq [w workers]
        (edn-dump (:out-file w)))))

(defnp emw-email-consider
  "[w (writer) task (email info)]
  Decide whether this writer should include this email in
  the batch, given its spam score and how it will affect the
  overall spam score of this writer's output and the running
  mean spam score of the most recent emails.

  Simply write to the writer's batch or ignore. Output is meaningless."

  [w task]

  (cond
    (> (:spam-score task) (:max-individual-spam-score env))
    (do #_(pln :email-indy-bad :w (:id w) :score (:spam-score task)))

    :default
    (let [stats @(:stats w)
          new-sum (+ (:spam-score task) (:score-sum stats))
          new-ct (inc (:em-ct stats))]
      (cond
        (> (/ new-sum new-ct)
          (:max-overall-spam-score env))
        (do #_(pln :overall-email-mean-bad :w (:id w) :score (:spam-score task)
                :new-mean (/ new-sum new-ct)
                :limit (:max-overall-spam-score env)))

        (not (p :running-mean (running-mean-ok? w (:spam-score task))))
        ;; todo save to "try later" array to be possibly
        ;; incorporated later when running mean might drop
        (do (pln :running-mean-bad (:spam-score task)))

        :default
        (when (dosync
                ;; todo make sure addr key matches generator when testing
                ;; or work out how to normalize keys in spec
                (let [addr (:email-address task)]
                  (when-not (get @(:addrs-hit w) addr)
                    (alter (:addrs-hit w) conj addr)
                    true)))
          (swap! (:stats w) merge {:em-ct     new-ct
                                   :score-sum new-sum})

          ;;; todo batch spits instead of spitting individually
          ;; (pln :sending-to (:id w) (:email-address task))
          (spit (:out-file w) task :append true))))))

(defn running-mean-ok?
  "[w (writer) new-score (score of email being considered)]
  Decide if this new score, if included, will violate running mean score
  invariants specified in config.edn"

  [w new-score]

  (let [stats @(:stats w)
        running-mean (:running-mean stats)
        new-ct (min
                 (:running-mean-span env)
                 (inc (:em-ct stats)))
        new-mean (+ running-mean
                   (/ (- new-score running-mean) new-ct))]
    (when (<= new-mean (:running-mean-max env))
      (swap! (:stats w) merge {:em-ct        new-ct
                               :running-mean new-mean})
      true)))

;;; --- utilities -------------------------------------------------

(defn bulk-input-build [prefix em-count]
  (let [bf (str "bulkinput/" prefix "-" em-count ".edn")]
    (spit bf {:build-date (.toString (java.util.Date.))
              :count      em-count})
    (doseq [em (email-records-test-gen em-count)]
      (spit bf em :append true))))

(defn bulk-input-sequence [path handler]
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader path))]
    (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
      (dorun (map handler (take-while (partial not= :fini) edn-seq))))))

;; (bulk-input-sequence "bulkinput/emf-10.edn" println)

#_(bulk-input-build "emf" 100)

(defn edn-load [path]
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader path))]
    (edn/read in)))

(defn edn-dump                                              ;; todo turn into validator
  ([path]
   (edn-dump path (str "edn-dumping " path)))

  ([path banner]
   (println banner)
   (with-open [in (java.io.PushbackReader. (clojure.java.io/reader path))]
     (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
       (dorun (map println (take-while (partial not= :fini) edn-seq)))))))

(defn pln [& args]
  (locking *out*
    (println (str/join " " args))))