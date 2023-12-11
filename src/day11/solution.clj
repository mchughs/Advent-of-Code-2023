(ns day11.solution
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

;; solution part 1

(defn transpose [m]
  (apply mapv vector m))

(defn expand-row [input]
  (reduce
   (fn [acc row]
     (if (every? #(= \. %) row)
       (conj acc row row)
       (conj acc row)))
   []
   input))

(defn- expand-space [input]
  (->> input
       expand-row
       transpose
       expand-row
       transpose))

(defn- manhattan-distance [[[x1 y1] [x2 y2]]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(time
 (let [input (expand-space (string/split-lines (slurp "src/day11/input.txt")))
       galaxies (->> input
                     (map-indexed vector)
                     (reduce (fn [acc [y line]]
                               (into acc
                                     (comp
                                      (map-indexed vector)
                                      (filter (fn [[_ tile]] (= \# tile)))
                                      (map first)
                                      (map (fn [x] [x y])))
                                     line))
                             #{}))]
   (transduce
    (map manhattan-distance)
    +
    0
    (combo/combinations galaxies 2))))

;; solution part 2

 