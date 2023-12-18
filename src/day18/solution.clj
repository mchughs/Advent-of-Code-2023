 (ns day18.solution
  (:require [clojure.string :as string]
            [clojure.core.matrix :as matrix]))

 ;; solution part 1

(def dir->vec
  {"U" [0 1]
   "L" [-1 0]
   "D" [0 -1]
   "R" [1 0]})

(defn- parse [s]
  (let [[direction paces _] (string/split s #" ")]
    {:direction direction
     :paces (Integer. paces)}))

(defn shift [n]
  (into [(last n)] (butlast n)))

(defn poly-area
  "See shoelace formula: https://en.wikipedia.org/wiki/Shoelace_formula"
  [xs ys]
  (* 0.5 (abs (- (matrix/dot xs (shift ys))
                 (matrix/dot ys (shift xs))))))

(defn- get-trench [input]
  (reduce (fn [path {:keys [direction paces]}]
            (let [delta (mapv * (dir->vec direction) [paces paces])]
              (->> (last path)
                   (mapv + delta)
                   (conj path))))
          [[0 0]]
          input))

(defn calculate-volume [input]
  (let [trench (get-trench input)
        area (poly-area (map first trench)
                        (map last trench))
        exterior-points (transduce (map :paces) + 0 input)
        ;; See Pick's theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem#Formula
        interior-points (- (inc area) (quot exterior-points 2))]
    (long (+ interior-points exterior-points))))

;; ~30ms
(time
 (let [input (map parse (string/split-lines (slurp "src/day18/input.txt")))]
   (calculate-volume input)))

 ;; solution part 2

(def decode
  {\0 "U"
   \1 "L"
   \2 "D"
   \3 "R"})

(defn parse* [s]
  (let [[_ _ z] (string/split s #" ")]
    {:direction (decode (get z 7))
     :paces (-> z
                (subs 2 7)
                (Integer/parseInt 16))}))

;; ~30ms
(time
 (let [input (map parse* (string/split-lines (slurp "src/day18/input.txt")))]
   (calculate-volume input)))
