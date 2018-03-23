(ns spamgen.core
  (:gen-class)
  (:require
    [config.core :refer [env]]
    [clojure.string :as str]
    [clojure.pprint :as pp]
    [clojure.edn :as edn]
    [clojure.core.async :refer [chan go-loop <! <!! >!!
                                timeout alt!!]]
    [clojure.tools.cli :refer [parse-opts] :as cli]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
    [taoensso.timbre :as log]
    [spamgen.genlist :refer :all]))

(def spamgen-cli
  [["-t" "--test TESTCOUNT" "Number of test email records to process ignoring file arg"
    :id :test-count
    :default 100
    ;; todo handle inputs like 100k
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 1 %) "Must be a number greater than one (1)"]]
   ["-v" "--verbose"]

   ["-h" "--help"]])

(def env-hack
  ;; todo persuade lein bin and config to work together
  {
   :smtp              ["1.2.3.4" "10.20.30.40" "100.200.101.201"
                       "11.22.33.44" "111.112.113.114" "22.33.44.55"]
   :worker-ct         6
   :individual-max    0.3
   :overall-mean-max  0.05
   :last-n-mean-max   0.1
   :last-n-span       100
   :bulkmail-out-path "bulkmail"
   })

(declare pln email-stream-to-sendfiles-mp)

#_(-main "-t30000" "-v")

(defn -main [& args]
  ;; uncomment during development so errors get through when async in play
  #_(Thread/setDefaultUncaughtExceptionHandler
      (reify Thread$UncaughtExceptionHandler
        (uncaughtException [_ thread ex]
          (log/error {:what      :uncaught-exception
                      :exception ex
                      :where     (str "Uncaught exception on" (.getName thread))}))))

  (let [input (cli/parse-opts args spamgen-cli)
        {:keys [options arguments summary errors]} input
        {:keys [verbose test-count help]} options]

    (cond
      errors (doseq [e errors]
               (println e))

      help (println "\nUsage:\n\n    spamgen <input-edn> options*\n\n"
             "Options:\n" (subs summary 1))

      :default (email-stream-to-sendfiles-mp
                 (email-records-test-gen test-count)
                 verbose)))

  ;; WARNING: comment this out for use with REPL. Necessary, to
  ;; get standalone version to exit reliably.
  ;;
  ;;(shutdown-agents)
  )

(declare email-stream-to-sendfiles
  emw-email-consider
  span-mean-ok
  edn-dump pln)

(defn email-batch-to-sendfiles [batch-input-path]
  (with-open [in (java.io.PushbackReader. (clojure.java.io/reader batch-input-path))]
    (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
      (email-stream-to-sendfiles
        (take-while (partial not= :fini) edn-seq)))))


(defn email-stream-to-sendfiles-mp
  "[email-stream (coll)]

  Produce one or more output files targeted
  for different SMTP servers constraining the sequence of emails
  in each to honor spam score constraints specified in config.edn
  and never to include two emails to the same address across all
  batches."
  [email-stream verbose]

  (let [em-addrs-hit (ref #{})
        workers (map (fn [id smtp-ip]
                       {:id        id
                        :smtp-ip   smtp-ip
                        :ch        (chan)
                        :addrs-hit em-addrs-hit
                        :out-file  (str
                                     (:bulkmail-out-path env-hack) "/em"
                                     smtp-ip ".txt")
                        :stats     (atom {:sent-ct                 0
                                          :last-n-mean           0
                                          :spam-score-sum        0
                                          :rejected-score        0
                                          :rejected-dup-addr     0
                                          :rejected-overall-mean 0
                                          :rejected-span-mean    0})})
                  (range)
                  (take (min (count (:smtp env-hack))
                          (:worker-ct env-hack))
                    (:smtp env-hack)))

        work-procs (dorun
                     (map (fn [w]
                            (go-loop []
                              (when-let [task (<! (:ch w))]
                                (emw-email-consider w task)
                                (recur))))
                       workers))]

    ;; --- initialize spit files for latter appends, including now a header

    (doseq [w workers]
      (spit (:out-file w)
        {:run-date (.toString (java.util.Date.))
         :smtp-ip  (:smtp-ip w)}))

    (p :feed-workers
      (dorun
        (map (fn [worker em-rec]
               (>!! (:ch worker) em-rec))
          (cycle workers)
          email-stream)))

    (pln :waiting-on-workers)

    (loop [[work-proc & rest :as ps] work-procs]
      (when work-proc
        (when-let [out (alt!!
                         (timeout 100) :timeout
                         work-proc ([r] r))]
          (recur rest))))

    (when verbose
      (doseq [w workers]
        (pln)
        (pln (format "worker %d:" (:id w)))
        (pp/pprint @(:stats w))
        (pln)))

    (pln :summary)
    (pp/pprint (apply merge-with +
                 (map #(select-keys @(:stats %)
                         [:sent-ct :rejected-score :rejected-dup-addr
                          :rejected-overall-mean
                          :rejected-span-mean])
                   workers)))

    (pln)
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

  ;;(pln :consider (:spam-score task)(:email-address task))

  (cond
    (> (:spam-score task) (:individual-max env-hack))
    (do
      (swap! (:stats w) update-in [:rejected-score] inc)
      #_(pln :email-indy-bad :w (:id w) :score (:spam-score task)))

    :default
    (let [stats @(:stats w)
          new-sum (+ (:spam-score task) (:spam-score-sum stats))
          new-ct (inc (:sent-ct stats))]
      (cond
        (> (/ new-sum new-ct)
          (:overall-mean-max env-hack))
        (do (swap! (:stats w) update-in [:rejected-overall-mean] inc)
            #_(pln :overall-email-mean-bad :score (:spam-score task)
                :new-mean (/ new-sum new-ct)
                :limit (:overall-mean-max env-hack)))

        (not (p :span-mean (span-mean-ok w (:spam-score task))))
        ;; todo save to "try later" array to be possibly
        ;; incorporated later when running mean might drop
        (do (swap! (:stats w) update-in [:rejected-span-mean] inc)
            #_(pln :span-mean-bad (:spam-score task)))

        :default
        (when (dosync
                ;; todo make sure addr key matches generator when testing
                ;; or work out how to normalize keys in spec
                (let [addr (:email-address task)]
                  (if (get @(:addrs-hit w) addr)
                    (do
                      (swap! (:stats w) update-in [:rejected-dup-addr] inc)
                      false)
                    (do
                      (alter (:addrs-hit w) conj addr)
                      true))))
          (swap! (:stats w) merge {:sent-ct          new-ct
                                   :spam-score-sum new-sum})

          ;;; todo batch spits instead of spitting individually?
          #_(pln :sending-to (:id w) (:spam-score task)
              :mean (/ new-sum new-ct))
          (spit (:out-file w) task :append true))))))

(defn span-mean-ok
  "[w (writer) new-score (score of email being considered)]
  Decide if this new score, if included, will violate running mean score
  invariants specified in config.edn"

  [w new-score]

  (let [stats @(:stats w)
        last-n-mean (:last-n-mean stats)
        new-ct (min
                 (:last-n-span env-hack)
                 (inc (:sent-ct stats)))
        new-mean (+ last-n-mean
                   (/ (- new-score last-n-mean) new-ct))]
    (if (<= new-mean (:last-n-mean-max env-hack))
      (do
        (swap! (:stats w) assoc :last-n-mean new-mean)
        #_(pln :okspan new-ct new-mean new-score (:last-n-mean-max env-hack))
        true)
      (do
        #_(pln :failspan new-ct new-mean (:last-n-mean-max env-hack))
        false))))

;;; --- utilities -------------------------------------------------

(defn bulk-input-build [prefix em-count]
  (let [bf (str "bulkinput/" prefix "-" em-count ".edn")]
    (spit bf {:build-date (.toString (java.util.Date.))
              :count      em-count})
    (doseq [em (email-records-test-gen em-count)]
      (spit bf em :append true))))

#_(bulk-input-build "emf" 100)

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