(ns day21.solution
  (:require [clojure.string :as string]))

 ;; solution part 1

(defn neighbors-fn [input]
  (let [[max-x max-y] ((juxt (comp count first) count) input)]
    (fn [[x y]]
      (->> [[(inc x) y]
            [(dec x) y]
            [x (inc y)]
            [x (dec y)]]
           (filter (fn [[x' y']] (and (< -1 x' max-x)
                                      (< -1 y' max-y))))
           (remove (fn [[x' y']] (= \# (get-in input [y' x']))))))))

(defn step-fn [neighbors]
  (fn [points]
    (into #{} (mapcat neighbors) points)))

(time
 (let [input (string/split-lines (slurp "src/day21/input.txt"))
       steps 64
       neighbors (neighbors-fn input)
       step (step-fn neighbors)
       start (->> input
                  (map-indexed (fn [y line]
                                 (map-indexed (fn [x c]
                                                (when (= \S c)
                                                  [x y])) line)))
                  flatten
                  (remove nil?))]
   (->> #{start}
        (iterate step)
        (take (inc steps))
        last
        count)))

 ;; solution part 2
