(ns day12.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

;; solution part 1

(defn parse [s]
  (let [[condition runs] (string/split s #" ")]
    {:condition condition
     :runs (->> runs
                (re-seq #"\d+")
                (map #(Integer. %)))
     :blanks (count (filter #(= \? %) condition))
     :damaged (count (filter #(= \# %) condition))}))

(defn replace-blanks [s coll]
  (loop [s s
         [x & xs] (map str coll)]
    (if (nil? x)
      s
      (recur (string/replace-first s #"\?" x)
             xs))))

(defn valid? [runs s]
  (->> (string/split s #"\.")
       (remove empty?)
       (map count)
       (= runs)))

(defn count-possibilities [{:keys [condition runs blanks damaged]}]
  (let [run-sum (apply + runs)
        missing-damaged (- run-sum damaged)
        missing-operational (- blanks missing-damaged)
        substitutes (combo/permutations (concat (repeat missing-operational \.)
                                                (repeat missing-damaged \#)))]
    (->> substitutes
         (map #(replace-blanks condition %))
         (filter (partial valid? runs))
         count)))

;; Brute force solution, takes about 6 seconds for the input

(time
 (with-open [rdr (io/reader "src/day12/input.txt")]
   (->> rdr
        line-seq
        (transduce (comp (map parse)
                         (map count-possibilities))
                   +
                   0))))

;; solution part 2

(defn parse* [s]
  (let [fold-factor 5
        [condition runs] (string/split s #" ")
        condition (string/join "?" (repeat fold-factor condition))]
    {:condition condition
     :runs (->> runs
                (re-seq #"\d+")
                (map #(Integer. %))
                (repeat fold-factor)
                (apply concat))
     :blanks (count (filter #(= \? %) condition))
     :damaged (count (filter #(= \# %) condition))}))

(def valid?* (memoize valid?))

(def solve
  (memoize
   (fn [{:keys [condition runs blanks]}]
     (let [[char & _] condition
           [run & remaining-runs] runs
           length (count condition)]
       (cond
         (nil? runs) (if (some #(= \# %) condition) 0 1)
         (empty? condition) 0
         (< length run) 0
         (zero? blanks) (if (valid?* runs condition) 1 0)
         (= \. char) (solve {:condition (subs condition 1)
                             :runs runs
                             :blanks blanks})
         (= \# char) (let [condition' (subs condition 0 run)]
                       (cond
                         (some #(= \. %) condition') 0
                         (= \# (get condition run)) 0
                         :else
                         (let [delta-blanks (count (filter #(= \? %) condition'))]
                           (solve {:condition (subs condition (min (inc run) length))
                                   :runs remaining-runs
                                   :blanks (- blanks delta-blanks)}))))
         (= \? char) (+ (solve {:condition (string/replace-first condition #"\?" ".") ;; pretend it's a dot
                                :runs runs
                                :blanks (dec blanks)})
                        (solve {:condition (string/replace-first condition #"\?" "#") ;; pretend it's a hash
                                :runs runs
                                :blanks (dec blanks)})))))))

;; Dynamic programming solution, takes about 9 seconds for the input from a cold start

(time
 (with-open [rdr (io/reader "src/day12/input.txt")]
   (->> rdr
        line-seq
        (transduce (comp (map parse*)
                         (map solve))
                   +
                   0))))
