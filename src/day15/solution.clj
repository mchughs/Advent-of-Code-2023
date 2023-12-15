(ns day15.solution
  (:require [clojure.string :as string]))

;; solution part 1

(defn run-hash [s]
  (reduce
   #(rem (* 17 (+ %1 (int %2))) 256)
   0
   s))

(time
 (let [input (string/split (string/trim (slurp "src/day15/input.txt")) #",")]
   (transduce
    (map run-hash)
    +
    0
    input)))

;; solution part 2

