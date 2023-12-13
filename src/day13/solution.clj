(ns day13.solution
  (:require [clojure.string :as string]))

;; solution part 1

(defn transpose [m]
  (apply mapv vector m))

(defn valid-reflection? [matrix point]
  (every? identity
          (for [a (range (count matrix))
                b (range (count matrix))
                :when (and
                       (< a b)
                       (= (abs (- a point))
                          (abs (- b point))))]
            (= (get matrix a)
               (get matrix b)))))

(defn calculate-reflection [validity-fn matrix]
  (let [horizontal-reflection-point
        (->> (range 0.5 (- (count matrix) 0.5) 1)
             (filter (partial validity-fn matrix))
             first)]
    (if horizontal-reflection-point
      (* 100 (Math/round horizontal-reflection-point))
      (let [transposed (transpose matrix)
            vertical-reflection-point
            (->> (range 0.5 (- (count transposed) 0.5) 1)
                 (filter (partial validity-fn transposed))
                 first)]
        (Math/round vertical-reflection-point)))))

(time
 (let [input (map string/split-lines (string/split (slurp "src/day13/input.txt") #"\n\n"))]
   (transduce
    (map (partial calculate-reflection valid-reflection?))
    + 0 input)))

;; solution part 2

(defn fuzzy= [ma mb]
  (->> (map vector ma mb)
       (remove (fn [[va vb]] (= va vb)))
       count
       (= 1)))

(defn valid-reflection?* [matrix point]
  (let [{:keys [still-valid? smudge-fixed?]}
        (->> (for [a (range (count matrix))
                   b (range (count matrix))
                   :when (and
                          (< a b)
                          (= (abs (- a point))
                             (abs (- b point))))]
               [a b])
             (reduce
              (fn [{:keys [still-valid? smudge-fixed?]} [a b]]
                (let [ma (get matrix a)
                      mb (get matrix b)]
                  (cond
                    (not still-valid?)
                    (reduced {:still-valid? false})
                    (= ma mb)
                    {:still-valid? true
                     :smudge-fixed? smudge-fixed?}
                    (and (fuzzy= ma mb) (not smudge-fixed?))
                    {:still-valid? still-valid?
                     :smudge-fixed? true}
                    (and (fuzzy= ma mb) smudge-fixed?)
                    (reduced {:still-valid? false}))))
              {:still-valid? true
               :smudge-fixed? false}))]
    (and still-valid? smudge-fixed?)))

(time
 (let [input (map string/split-lines (string/split (slurp "src/day13/input.txt") #"\n\n"))]
   (transduce
    (map (partial calculate-reflection valid-reflection?*))
    + 0 input)))
