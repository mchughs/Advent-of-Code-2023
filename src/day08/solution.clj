(ns day08.solution
  (:require [clojure.string :as string]))

;; solution part 1

(def start "AAA")
(def end "ZZZ")

(time
 (let [[lr _ & remaining] (->> (slurp "src/day08/input.txt")
                               string/split-lines)
       nodes (reduce (fn [acc s]
                       (let [[node left right] (re-seq #"[A-Z]{3}" s)]
                         (assoc acc node {\L left \R right})))
                     {}
                     remaining)
       instructions (cycle lr)]
   (loop [[instruction & remaining] instructions
          steps 0
          loc start]
     (if (= loc end)
       steps
       (recur remaining (inc steps) (get-in nodes [loc instruction]))))))

;; solution part 2

