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

(time
 (let [input (string/split-lines (slurp "src/day14/input.txt"))]
   (->> input
        tilt-up
        reverse
        (transduce
         (map-indexed (fn [idx line]
                        (* (inc idx)
                           (count (filter #{\O} line)))))
         +
         0))))

;; solution part 2

