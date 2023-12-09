(ns day09.solution
  (:require [clojure.java.io :as io]))

;; solution part 1

(defn extract-input [s]
  (map #(Integer. %) (re-seq #"-?\d+" s)))

(defn step [history]
  (->> history
       (partition 2 1)
       (map (fn [[a b]]
              (- b a)))))

(defn extrapolate-forward [history]
  (let [steps
        (loop [curr history
               steps []]
          (if (every? zero? curr)
            steps
            (recur (step curr) (conj steps curr))))]
    (reduce #(+ %1 (last %2)) 0 steps)))

(time
 (with-open [rdr (io/reader "src/day09/input.txt")]
   (->> rdr
        line-seq
        (transduce (comp (map extract-input)
                         (map extrapolate-forward))
                   +
                   0))))

;; solution part 2

(defn extrapolate-backward [history]
  (let [steps
        (loop [curr history
               steps []]
          (if (every? zero? curr)
            steps
            (recur (step curr) (conj steps curr))))]
    (transduce
     (comp (map first)
           (map-indexed vector))
     (completing
      (fn [acc [idx x]]
        (let [f (if (even? idx) + -)]
          (f acc x))))
     0
     steps)))

(time
 (with-open [rdr (io/reader "src/day09/input.txt")]
   (->> rdr
        line-seq
        (transduce (comp (map extract-input)
                         (map extrapolate-backward))
                   +
                   0))))
