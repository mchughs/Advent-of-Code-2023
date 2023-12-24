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

;; solution part 2

(defn dot-product
  [[x y z] [x' y' z']]
  (+ (* x x')
     (* y y')
     (* z z')))

(defn cross-product
  [[x y z] [x' y' z']]
  [(- (* y z') (* z y'))
   (* -1 (- (* x z') (* z x')))
   (- (* x y') (* y x'))])

(defn squared-magnitude [vector]
  (reduce + (map #(* % %) vector)))

;; Just used to be able to verify that two , 3D lines intersect at a positive time, t.
(defn solve-t
  " Jus"
  [[px py pz vx vy vz]
   [px' py' pz' vx' vy' vz']]
  (let [c (cross-product [vx vy vz] [vx' vy' vz'])
        s (squared-magnitude c)]
    (when-not (zero? s)
      (/ (dot-product
          [(- px' px) (- py' py) (- pz' pz)]
          (cross-product [vx' vy' vz'] c))
         s))))

(time
 (let [input (-> "src/day24/input.txt" slurp string/split-lines)
       hailstones (map parse input)
       solution [133619443970450 263917577518425 180640699244168 314 19 197]]
   ;; Will long overflow for the given input unfortunately.
   (map #(solve-t solution %) hailstones)))

(+ 133619443970450 263917577518425 180640699244168)

;; Solved by taking 3 linearly independent lines (the first 3 of the input were fine).
;; Then created a system of 9 equations and 9 unknowns as shown below.
;; Used chatGPT to generate some python code to solve the system of equations using sympy.
;; Could probably also use https://github.com/clojure-numerics/expresso/blob/master/src/main/clojure/numeric/expresso/solve.clj#L322
;; for a clojure solver.

;; t1 = x
;; t2 = y
;; t3 = z

;; px = a
;; py = b
;; pz = c
;; vx = d
;; vy = e
;; vz = f

;; 213004023520250 + 118 * x = a + d * x
;; 255007063487325 + 41 * x = b + e * x
;; 286351797522218 - 64 * x = c + f * x
;; 488850102886640 - 73 * y = a + d * y
;; 357544262814165 - 83 * y = b + e * y
;; 194409329434718 + 182 * y = c + f * y
;; 193401607687542 + 133 * z = a + d * z
;; 253348355203801 + 51 * z = b + e * z
;; 213339230780036 + 98 * z = c + f * z
