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

(defn matches-runs-fn? [runs]
  (fn [s]
    (if (empty? s)
      (empty? runs)
      (->> (string/split s #"\.")
           (remove empty?)
           (map count)
           (= runs)))))

(defn count-possibilities [{:keys [condition runs blanks damaged]}]
  (let [run-sum (apply + runs)
        missing-damaged (- run-sum damaged)
        missing-operational (- blanks missing-damaged)
        substitutes (combo/permutations (concat (repeat missing-operational \.)
                                                (repeat missing-damaged \#)))]
    (->> substitutes
         (map #(replace-blanks condition %))
         (filter (matches-runs-fn? runs))
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
