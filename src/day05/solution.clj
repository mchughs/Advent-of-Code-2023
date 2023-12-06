(ns day05.solution
  (:require [clojure.string :as str]))

;; solution part 1

(defn- build-maps [ranges]
  (->> ranges
       (map (fn [[destination-range-start source-range-start range-length]]
              (memoize
               (fn [source]
                 (let [delta (- source source-range-start)]
                   (if (< -1 delta range-length)
                     (+ destination-range-start delta)
                     source))))))))

(defn- calc-destination [maps seed]
  (reduce (memoize
           (fn [loc fns]             
             (reduce (fn [x f]
                       (let [y (f x)]
                         (if (= x y)
                           x
                           (reduced y))))
                     loc
                     fns)))
          seed
          maps))

(time
 (let [input (as-> (slurp "src/day05/input.txt") $
               (str/split $ #"\n\n")
               (map (fn [s] (map #(Long. %) (re-seq #"\d+" s))) $))
       [seeds & maps] input
       maps (->> maps
                 (map #(partition 3 %))
                 (map build-maps))]
   (apply min (map #(calc-destination maps %) seeds))))
