(ns day24.solution
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

;; solution part 1

(defn parse [s]
  (let [[px py pz vx vy vz] (map #(Long. %) (re-seq #"-?\d+" s))]
    [px py pz vx vy vz]))

(defn intersect-in-test-area-fn? [minimum maximum]
  (fn [[[px py _ vx vy _]
        [px' py' _ vx' vy' _]]]
    ;; Looking at the input there are no hailstones with 0
    ;; x velocity so we don't have to worry about the infinite slope case
    (let [forwards-x? (if (pos? vx) <= >=)
          forwards-x?' (if (pos? vx') <= >=)
          forwards-y? (if (pos? vy) <= >=)
          forwards-y?' (if (pos? vy') <= >=)
          m (/ vy vx)
          m' (/ vy' vx')
          parallel? (= m m')]
      (when-not parallel? ;; checked and there are no parallel lines in the input inline with each other
        (let [x (/ (+ (* m px)
                      (* m' px' -1)
                      py'
                      (* py -1))
                   (- m m'))
              y (+ (* m (- x px))
                   py)
              [x y] [(float x) (float y)]]
          (and (<= minimum x maximum)
               (<= minimum y maximum)
               (forwards-x? px x)
               (forwards-y? py y)
               (forwards-x?' px' x)
               (forwards-y?' py' y)))))))

;; ~500ms
(time
 (let [input (-> "src/day24/input.txt" slurp string/split-lines)
       hailstones (map parse input)
       combinations (combo/combinations hailstones 2)
       intersect-in-test-area? (intersect-in-test-area-fn?
                                200000000000000
                                400000000000000)]
   (->> combinations
        (map intersect-in-test-area?)
        (filter identity)
        count)))

