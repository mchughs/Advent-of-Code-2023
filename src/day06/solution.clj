(ns day06.solution
  (:require [clojure.string :as string]))

;; solution part 1

(defn quadratic-formula [a b c]
  [(/ (+ (* -1 b) (Math/sqrt (- (* b b) (* 4 a c))))
      (* 2 a))
   (/ (- (* -1 b) (Math/sqrt (- (* b b) (* 4 a c))))
      (* 2 a))])

(defn ints-in-interval [[a b]]
  (inc (- (Math/floor b) (Math/ceil a))))

(defn ways-to-win [time distance]
  ;; finding endpoint solutions to equation -(hold-time)^2 + (total-time)*(hold-time) - (total-distance + 1) >= 0
  ;; once we know the interval we calculate the number of integers in that interval to find the total number of solutions
  (ints-in-interval (quadratic-formula -1 time (* -1 (inc distance)))))

(time
 (let [input (->> (slurp "src/day06/input.txt")
                  string/split-lines
                  (map (fn [s] (map #(Integer. %) (re-seq #"\d+" s))))
                  (apply zipmap))]
   (reduce-kv (fn [acc time distance]
                (* acc (ways-to-win time distance)))
              1
              input)))

;; solution part 2

(time
 (let [input (->> (slurp "src/day06/input.txt")
                  string/split-lines
                  (map #(apply str (re-seq #"\d+" %)))
                  (map #(Long. %)))]
   (ways-to-win (first input) (last input))))
