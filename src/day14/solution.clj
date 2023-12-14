(ns day14.solution
  (:require [clojure.string :as string]))

;; solution part 1

(defn transpose [m]
  (apply mapv vector m))

(defn tilt-left [m]
  (map (fn [line]
         (->> line
              (partition-by #(= \# %))
              (mapcat (comp reverse sort))))
       m))

(def tilt-up (comp transpose tilt-left transpose))

(defn measure-load [m]
  (->> m
       reverse
       (transduce
        (map-indexed (fn [idx line]
                       (* (inc idx)
                          (count (filter #{\O} line)))))
        +
        0)))

(time
 (let [input (string/split-lines (slurp "src/day14/input.txt"))]
   (->> input
        tilt-up
        measure-load)))

;; solution part 2

(defn tilt-right [m]
  (map (fn [line]
         (->> line
              (partition-by #(= \# %))
              (mapcat sort)))
       m))

(def tilt-down (comp transpose tilt-right transpose))

(def run-cycle  #(-> % tilt-up tilt-left tilt-down tilt-right))

;; Different 'cycle' than the one in run-cycle
(defn get-cycle
  "(get-cycle '(1 2 3 4 5 3 4 5 ...))
   =>
   [2 4]"
  [coll]
  (reduce (fn [acc [idx x]]
            (if-let [v (get acc x)]
              (reduced (conj v idx))
              (assoc acc x [idx])))
          {}
          (map-indexed #(vector % %2) coll)))

(time
 (let [input (string/split-lines (slurp "src/day14/input.txt"))
       n 1000000000
       [start end] (->> input
                        (iterate run-cycle)
                        get-cycle)
       equivalent-cycle (+ (mod (- n start)
                                (- end start))
                           start)]
   (->> input
        (iterate run-cycle)
        (take (inc equivalent-cycle))
        last
        measure-load)))
