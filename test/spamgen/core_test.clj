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

(defn no-can-dart
  "Given a nmber of darts and the scores available on a dartboard, return
  the lowest total score that cannot be scored. e.g., with one dart and
  scores 1 and 3, we cannot score two. Withe two darts we cannot score 5.

  Assumes scores are positive."

  [dart-ct dartboard-scores]

  (let [scores (sort (distinct dartboard-scores))           ;; short for normalized scores
        gapless (atom 0)]                                   ;; short for gapless max score observed as we traverse possible scoring combos
    (cond
      ;;; nail a couple of special cases
      (or
        (not (pos? dart-ct))
        (empty? dartboard-scores)
        (> (first scores) 1))
      1

      ;; this one actually spares us a full sweep
      (= dartboard-scores (range 1 (inc (count dartboard-scores))))
      (inc (* dart-ct (last scores)))

      :default
      (letfn [(next-in-order []
                (inc @gapless))
              (throw-dart [dart-no score-so-far]
                (when (pos? dart-no)
                  (loop [[throw-score & rthrows] scores]
                    (when throw-score
                      (let [new-total (+ score-so-far throw-score)]
                        ;; we could pull the recursive call here since all branches of the cond
                        ;; invoke it, but methinks that borders on obfuscation
                        (cond
                          (= new-total (next-in-order))
                          (do
                            (swap! gapless inc)
                            ;; we keep going as an efficiency: if the next slice is one more
                            ;; than the current slice, we can bump our gapless value straight away,
                            ;; effectively pruning further searching.
                            (throw-dart (dec dart-no) new-total)
                            (recur rthrows))

                          (> new-total (next-in-order))
                          ;; rthrows, being sorted, contains only higher values, so abandon this dart
                          ;; and try the next, if any, which will possibly have access to lower values.
                          (throw-dart (dec dart-no) new-total)

                          :default                          ;; ie, new-total too low
                          ;; try next dart if any and keep trying this dart
                          (do
                            (throw-dart (dec dart-no) new-total)
                            (recur rthrows))))))))]
        (throw-dart dart-ct 0)
        (next-in-order)))))

(deftest darts
  (let [board nil]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 1 (no-can-dart -1 board)))
    (is (= 1 (no-can-dart 10 board))))
  (let [board [2 3 4]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 1 (no-can-dart 1 board)))
    (is (= 1 (no-can-dart 10 board))))
  (let [board [1 2 3 4]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 5 (no-can-dart 1 board)))
    (is (= 9 (no-can-dart 2 board))))
  (let [board [1 3]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 2 (no-can-dart 1 board)))
    (is (= 5 (no-can-dart 2 board)))
    (is (= 8 (no-can-dart 3 board))))
  (let [board [1 3 7]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 2 (no-can-dart 1 board)))
    (is (= 5 (no-can-dart 2 board)))
    (is (= 12 (no-can-dart 3 board))))
  (let [board [1 4 7]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 2 (no-can-dart 1 board)))
    (is (= 3 (no-can-dart 2 board)))
    (is (= 10 (no-can-dart 3 board))))
  (let [board [1 2 5 6]]
    (is (= 1 (no-can-dart 0 board)))
    (is (= 3 (no-can-dart 1 board)))
    (is (= 9 (no-can-dart 2 board)))
    (is (= 15 (no-can-dart 3 board)))))

#_
(let [chunk 100
      total 40000]
  (let [chunks (partition-all chunk
                 (email-records-test-gen total))]
    (dorun
      (map (fn [n c]
             (spit (format "bulkinput/em-%d-%d.edn" total chunk)
               (into [] c) :append (pos? n)))
        (range)
        chunks))))

#_
(with-open [in (java.io.PushbackReader. (clojure.java.io/reader "bulkinput/em-12.edn"))]
  (let [edn-seq (repeatedly (partial edn/read {:eof :fini} in))]
    (doseq [chunk (take-while (partial not= :fini) edn-seq)]
      (pln :chk chunk))))

#_ (edn/read-string
     (slurp "bulkinput/em-1000.edn"))

(deftest slurptest
  (profile
    {:dynamic? true}
    (p :devtest-mp
      (email-file-to-sendfiles-mp
        "bulkinput/em-1000000-100.edn"
        false)))
  (pln :devtest-fini))

(deftest devtest
  (profile
    {:dynamic? true}
    (p :devtest-mp
      (email-stream-to-sendfiles-mp
        (email-records-test-gen 100000)
        false)))
  (pln :devtest-fini))

(deftest test-gen-count
  (profile
    {:dynamic? true}
    (p :test-gen-count
      (let [test (email-records-test-gen 10000)]
        (pln :testing!)
        (count test)))))

(deftest test-gen-apit
  (profile
    {:dynamic? true}
    (p :test-gen-count
      (spit "testgen.edn"
        (into [] (email-records-test-gen 100000))))))

(deftest test-gen-slurp
  (profile
    {:dynamic? true}
    (p :test-gen-count
      (slurp "testgen.edn"))))



;; todo add tests to read back in output and confirm constraints met

