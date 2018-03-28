(ns dartgen.core
  (:require [clojure.string :as str]))

(declare pln xpln)

(defn low-score-unattainable [darts board]
  (letfn [(scorable? [goal darts]
            (cond
              (zero? goal) true
              (or
                (zero? darts)
                (empty? board)
                (neg? goal)) false

              (some #{goal} board) true

              :default (some (fn [s]
                               (scorable? (- goal s) (dec darts)))
                         board)))]
    (some #(when-not (scorable? % darts) %)
      (range))))

(defn board-yielding-highest-unattainable
  "Given a dart board with 'slices' hittable scores not
  necessarily numbered in sequence between 1 and maxi inclusive
  and a number of darts, determine (kinda -- see next) the board scoring yielding
  the highest unattainable score.

  Actually just tries maxi between mini and maxi. If one sets maxi too
  low, higher unattainabes will be missed. The unattainable does max out
  if one sets the maxi high enough, but we have observed the unattainable
  levelling out for five maxis and then picking up again. Left as an exercise
  is determining when higher maxis will not yield a higher unattainable.

  Top-level playground follows code."
  ([] (board-yielding-highest-unattainable {:slices 5 :darts 3 :mini 2 :maxi 30}))

  ([options]
   (let [
         {:keys [slices darts mini maxi verbose]}
         (merge {:slices 5 :darts 3 :mini 2 :maxi 30} options)]

     (let [best (atom nil)]
       (letfn [(rboard [slices mini maxi board-so-far]
                 ;; when we reach 1 more than the max score possible,
                 ;; we are done:
                 (when-not (and @best
                              (= (first @best) (inc (* darts maxi))))

                 (cond
                   (zero? slices)
                   (let [r (low-score-unattainable darts board-so-far)]
                     (when (or (nil? @best)
                             (> r (first @best)))
                       (when verbose (pln :new-best! r board-so-far))
                       (reset! best [r board-so-far])))

                   :default
                   (doseq [score (range mini (inc maxi))]
                     (rboard (dec slices)
                       (inc score) maxi
                       (conj board-so-far score))))))]
         ;; as a convenience we force a "1" score (then adjust down the
         ;; number of slices) because without a score of 1 the lowest unattainable is 1.
         (rboard (dec slices) mini maxi [1])
         @best)))))

#_ (board-yielding-highest-unattainable)

#_;; playground
    (board-yielding-highest-unattainable {:slices 3
                                          :maxi   5})

(comment
  It seems we are building a DAG which can be traversed variously hitting all
  sums without a gap. At a certain point, increasing the max value does not help because
  we have to start at one (lest that be the lowest unattainable) so max values higher
  than the number of slices means gaps must be endured, and with too high a max
  we will create a gap that cannot be bridged.

  The best scorings are reminiscent of the Fibonacci series, which makes
  sense because that climbs quickly but not too quickly, which would increase
  the risk of an unbridgable gap.

  What is tricky is projecting how new higher maxes, by removing the prior max, creates
  a gap in the space already established.)

;; --- utils -------------

(defn pln [& args]
  (locking *out*
    (println (str/join " " args))))

(defn xpln [& args])